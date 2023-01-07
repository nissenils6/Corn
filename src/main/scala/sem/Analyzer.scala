package sem

import core.*
import syn.*

private def reduceOptionalErrors(errors: List[Option[CompilerError]]): Option[CompilerError] = errors.flatten match {
  case Nil => None
  case _ => Some(errors.reduce(_ | _))
}

private def resolveTypeExprRecursive(module: Module, typeExpr: TypeExpr, visited: List[(TypeGlobalStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeExpr match {
  case UnitTypeExpr(_) => Right(UnitDatatype)
  case IdenTypeExpr(iden, range) => for {
    typeVar <- module.types.get(iden).toRight(Error.semantic(s"Could not resolve '$iden' as a type", range))
    datatype <- resolveTypeVar(module, typeVar, range, visited)
  } yield datatype
  case TupleTypeExpr(elements, _) => for {
    datatypes <- elements.map(typeExpr => resolveTypeExprRecursive(module, typeExpr, visited)).extract.mapLeft(_.reduce(_ | _))
  } yield TupleDatatype(datatypes, false)
  case MutTypeExpr(typeExpr, range) => ???
  case RefTypeExpr(typeExpr, range) => ???
  case FunTypeExpr(params, returnType, range) => ???
}

private def resolveTypeVar(module: Module, typeVar: TypeVar, cause: FilePosRange, visited: List[(TypeGlobalStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeVar.value match {
  case Some(datatype) => Right(datatype)
  case None =>
    val typeStmt = typeVar.stmt.get
    val index = visited.indexWhere(_._1 eq typeStmt)
    if (index >= 0) {
      val chain = ((typeStmt, cause) :: visited).take(index + 2).reverse
      val components = ErrorComponent(chain.head._2, Some(s"type '${chain.head._1.name}' is to be evaluated")) ::
        chain.tail.zip(chain).map { case ((cur, range), (cause, _)) =>
          ErrorComponent(range, Some(s"In order to evaluate type '${cause.name}', type '${cur.name}' must be evaluated"))
        }
      Left(Error(Error.SEMANTIC, module.file, components, Some(s"Cycle detected when trying to evaluate type '${typeStmt.name}'")))
    } else {
      for {
        datatype <- resolveTypeExprRecursive(module, typeStmt.typeExpr, (typeStmt, cause) :: visited)
      } yield {
        typeVar.value = Some(datatype)
        datatype
      }
    }
}

def resolveTypes(module: Module): Either[CompilerError, Unit] = for {
  _ <- module.typeStmts.map {
    case typeGlobalStmt: TypeGlobalStmt =>
      module.types.get(typeGlobalStmt.name) match {
        case Some(prior) =>
          val newDefinition = ErrorComponent(typeGlobalStmt.nameRange, Some(s"new definition of type '${typeGlobalStmt.name}'"))
          if (prior.stmt.nonEmpty) {
            val definedHere = ErrorComponent(prior.stmt.get.nameRange, Some(s"but '${prior.name}' is already defined here"))
            Left(Error(Error.SEMANTIC, module.file, List(newDefinition, definedHere), Some(s"multiple type definitions with the same name '${typeGlobalStmt.name}'")))
          } else {
            Left(Error(Error.SEMANTIC, module.file, List(newDefinition), Some(s"type definition with same name as builtin type '${typeGlobalStmt.name}'")))
          }
        case None =>
          module.types(typeGlobalStmt.name) = typeGlobalStmt.typeVar
          Right(())
      }
  }.extract.mapBoth(_.asInstanceOf[List[CompilerError]].reduce(_ | _), _ => ())

  _ <- module.typeStmts.map {
    case typeGlobalStmt: TypeGlobalStmt =>
      resolveTypeVar(module, typeGlobalStmt.typeVar, typeGlobalStmt.nameRange, List())
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
} yield ()

private def resolveTypeExpr(module: Module, typeExpr: TypeExpr): Either[CompilerError, Datatype] = typeExpr match {
  case UnitTypeExpr(_) => Right(UnitDatatype)
  case IdenTypeExpr(iden, _) => module.types.get(iden).map(_.value.get).toRight(Error.semantic(s"Could not resolve '$iden' as a type", range))
  case TupleTypeExpr(elements, _) => for {
    datatypes <- elements.map(e => resolveTypeExpr(module, e)).extract
  } yield TupleDatatype(datatypes, false)
  case MutTypeExpr(typeExpr, range) => ???
  case RefTypeExpr(typeExpr, range) => ???
  case FunTypeExpr(params, returnType, range) => ???
}

private def trivialTypeOf[T <: AnyVar](module: Module, pattern: Pattern[T]): Either[Option[CompilerError], Datatype] = pattern match {
  case VarPattern(_, typeExprOpt, _) => for {
    typeExpr <- typeExprOpt.toRight(None)
    datatype <- resolveTypeExpr(module, typeExpr).mapLeft(Some.apply)
  } yield datatype
  case TuplePattern(elements, _) => elements.map(p => trivialTypeOf(module, p)).extract.mapBoth(reduceOptionalErrors, datatypes => TupleDatatype(datatypes, false))
}

private def trivialTypeof(module: Module, expr: Expr): Either[Option[CompilerError], Datatype] = expr match {
  case CallExpr(function, _, _) => trivialTypeof(module, function).flatMap {
    case FunDatatype(_, returnType, _) => Right(returnType)
    case datatype => Left(Some(Error.semantic(s"Expected the expression to be invoked to be of a function type, found expression of type '$datatype'", function.range)))
  }
  case IdenExpr(_, _) => Left(None)
  case RefExpr(expr, _) => ???
  case ValExpr(expr, _) => ???
  case IntExpr(_, _) => Right(IntDatatype)
  case BoolExpr(_, _) => Right(BoolDatatype)
  case TupleExpr(elements, _) => elements.map(e => trivialTypeof(module, e)).extract.mapBoth(reduceOptionalErrors, datatypes => TupleDatatype(datatypes, false))
  case BlockExpr(_, expr, _) => trivialTypeof(module, expr)
  case UnitExpr(_) => Right(UnitDatatype)
  case DotExpr(expr, iden, _) => ???
  case FunExpr(parameterPatterns, returnTypeExpr, _, _) => for {
    params <- parameterPatterns.map(p => trivialTypeOf(module, p)).extract.mapLeft(reduceOptionalErrors)
    returnType <- resolveTypeExpr(module, returnTypeExpr).mapLeft(Some.apply)
  } yield FunDatatype(params, returnType, false)
  case IfExpr(_, ifBlock, elseBlock, _) => for {
    ifType <- trivialTypeof(module, ifBlock)
    elseType <- trivialTypeof(module, elseBlock)
    result <- (ifType, elseType) match {
      case (UnitDatatype, UnitDatatype) => Right(UnitDatatype)
      case (_, UnitDatatype) => Right(ifType)
      case (UnitDatatype, _) => Right(elseType)
      case _ if ifType == elseType => Right(ifType)
      case _ =>
        val ifBranch = ErrorComponent(ifBlock.range, Some(s"The if branch is of type '$ifType'"))
        val elseBranch = ErrorComponent(elseBlock.range, Some(s"The else branch is of type '$elseType'"))
        Left(Some(Error(Error.SEMANTIC, module.file, List(ifBranch, elseBranch), Some(s"Both branches of an if statement must return the same value"))))
    }
  } yield result
}

def visitIncompletePattern(module: Module, pattern: Pattern[GlobalConst], datatype: Datatype): Either[CompilerError, Unit] = pattern match {
  case pattern@VarPattern(name, typeExpr, _) => typeExpr match {
    case Some(typeExpr) => for {
      resolvedDatatype <- resolveTypeExpr(module, typeExpr)
      result <- if (datatype.isSubtypeOf(resolvedDatatype)) {
        val const = GlobalConst(name, resolvedDatatype)
        pattern.variable = const
        module.addConst(const)
        Right(())
      } else {
        Left(Error.semantic(s"Type mismatch, variable of type $resolvedDatatype does not match value of type $datatype", typeExpr.range))
      }
    } yield result
    case None =>
      val const = GlobalConst(name, datatype)
      pattern.variable = const
      module.addConst(const)
      Right(())
  }
  case TuplePattern(elements, range) => datatype match {
    case TupleDatatype(datatypes, mutable) => ???
    case _ => Left(Error.semantic("The pattern did not match the return type of the expression", range))
  }
}

def resolveConsts(module: Module): Either[CompilerError, Unit] = {
  val a = module.constStmts.map { constStmt =>
    if (constStmt.pattern.incomplete) {
      for {
        datatype <- trivialTypeof(module, constStmt.expr).mapLeft(_.getOrElse(Error.semantic("Constants without explicit type must be trivially type inferrable (The expression cannot reference other constants, use explicit type annotations where this is not possible)", constStmt.expr.range)))
        _ <- visitIncompletePattern(module, constStmt.pattern, datatype)
      } yield ()
    } else {
      Right(())
    }
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
  Right(())
}
