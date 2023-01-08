package sem

import core.*
import syn.*

private def setParentRecursive(parent: ConstAndTypeContainer)(expr: Expr): Unit = expr match {
  case CallExpr(function, args, _) => (function :: args).foreach(setParentRecursive(parent))
  case IdenExpr(_, _) => ()
  case RefExpr(expr, _) => setParentRecursive(parent)(expr)
  case ValExpr(expr, _) => setParentRecursive(parent)(expr)
  case IntExpr(_, _) => ()
  case BoolExpr(_, _) => ()
  case TupleExpr(elements, _) => elements.foreach(setParentRecursive(parent))
  case newParent@BlockExpr(stmts, expr, _) =>
    newParent.parent = Some(parent)
    stmts.foreach {
      case ExprStmt(expr, _) => setParentRecursive(newParent)(expr)
      case AssignVarStmt(_, expr, _) => setParentRecursive(newParent)(expr)
      case AssignRefStmt(refExpr, expr, _) =>
        setParentRecursive(newParent)(refExpr)
        setParentRecursive(newParent)(expr)
      case LocalVarStmt(_, expr, _) =>
        setParentRecursive(newParent)(expr)
      case LocalConstStmt(_, expr, _) =>
        setParentRecursive(newParent)(expr)
      case LocalTypeStmt(_, _, _, _) => ()
    }
    setParentRecursive(newParent)(expr)
  case UnitExpr(_) => ()
  case DotExpr(expr, _, _) => setParentRecursive(parent)(expr)
  case FunExpr(_, _, expr, _) => setParentRecursive(parent)(expr)
  case IfExpr(condition, ifBlock, elseBlock, _) =>
    setParentRecursive(parent)(condition)
    setParentRecursive(parent)(ifBlock)
    setParentRecursive(parent)(elseBlock)
}

def resolveScopeParents(module: Module): Unit = {
  module.varStmts.map(_.expr).foreach(setParentRecursive(module))
  module.constStmts.map(_.expr).foreach(setParentRecursive(module))
}

private def reduceOptionalErrors(errors: List[Option[CompilerError]]): Option[CompilerError] = errors.flatten match {
  case Nil => None
  case errors => Some(errors.reduce(_ | _))
}

private def resolveTypeExprRecursive(container: ConstAndTypeContainer, typeExpr: TypeExpr, visited: List[(TypeStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeExpr match {
  case UnitTypeExpr(_) => Right(UnitDatatype)
  case IdenTypeExpr(iden, range) => for {
    typeVar <- container.lookupType(iden, range)
    datatype <- resolveTypeVar(container, typeVar, range, visited)
  } yield datatype
  case TupleTypeExpr(elements, _) => for {
    datatypes <- elements.map(typeExpr => resolveTypeExprRecursive(container, typeExpr, visited)).extract.mapLeft(_.reduce(_ | _))
  } yield TupleDatatype(datatypes, false)
  case MutTypeExpr(typeExpr, range) => ???
  case RefTypeExpr(typeExpr, range) => ???
  case FunTypeExpr(params, returnType, range) => ???
}

private def resolveTypeVar(container: ConstAndTypeContainer, typeVar: TypeVar, causeRange: FilePosRange, visited: List[(TypeStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeVar.value match {
  case Some(datatype) => Right(datatype)
  case None =>
    val typeStmt = typeVar.stmt.get
    val index = visited.indexWhere(_._1 eq typeStmt)
    if (index >= 0) {
      val chain = ((typeStmt, causeRange) :: visited).take(index + 2).reverse
      val components = ErrorComponent(chain.head._2, Some(s"type '${chain.head._1.name}' is to be evaluated")) ::
        chain.tail.zip(chain).map { case ((cur, range), (cause, _)) =>
          ErrorComponent(range, Some(s"In order to evaluate type '${cause.name}', type '${cur.name}' must be evaluated"))
        }
      Left(Error(Error.SEMANTIC, causeRange.file, components, Some(s"Cycle detected when trying to evaluate type '${typeStmt.name}'")))
    } else {
      for {
        datatype <- resolveTypeExprRecursive(container, typeStmt.typeExpr, (typeStmt, causeRange) :: visited)
      } yield {
        typeVar.value = Some(datatype)
        datatype
      }
    }
}

private def resolveTypes(container: ConstAndTypeContainer): Either[CompilerError, Unit] = for {
  _ <- container.typeStmts.map { typeStmt =>
    container.addType(typeStmt.typeVar)
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
  _ <- container.typeStmts.map { typeStmt =>
    resolveTypeVar(container, typeStmt.typeVar, typeStmt.nameRange, List())
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
} yield ()

private def resolveTypeExpr(container: ConstAndTypeContainer, typeExpr: TypeExpr): Either[CompilerError, Datatype] = typeExpr match {
  case UnitTypeExpr(_) => Right(UnitDatatype)
  case IdenTypeExpr(iden, range) => container.types.get(iden).map(_.value.get).toRight(Error.semantic(s"Could not resolve '$iden' as a type", range))
  case TupleTypeExpr(elements, _) => for {
    datatypes <- elements.map(e => resolveTypeExpr(container, e)).extract.mapLeft(_.reduce(_ | _))
  } yield TupleDatatype(datatypes, false)
  case MutTypeExpr(typeExpr, range) => ???
  case RefTypeExpr(typeExpr, range) => ???
  case FunTypeExpr(params, returnType, range) => ???
}

private def trivialTypeOf[T <: AnyVar](container: ConstAndTypeContainer, pattern: Pattern[T]): Either[Option[CompilerError], Datatype] = pattern match {
  case VarPattern(_, typeExprOpt, _) => for {
    typeExpr <- typeExprOpt.toRight(None)
    datatype <- resolveTypeExpr(container, typeExpr).mapLeft(Some.apply)
  } yield datatype
  case TuplePattern(elements, _) => elements.map(p => trivialTypeOf(container, p)).extract.mapBoth(reduceOptionalErrors, datatypes => TupleDatatype(datatypes, false))
}

private def trivialTypeof(container: ConstAndTypeContainer, expr: Expr): Either[Option[CompilerError], Datatype] = expr match {
  case CallExpr(function, _, _) => for {
    funType <- trivialTypeof(container, function)
    newType <- funType match {
      case FunDatatype(_, returnType, _) => Right(returnType)
      case datatype => Left(Some(Error.semantic(s"Expected the expression to be invoked to be of a function type, found expression of type '$datatype'", function.range)))
    }
  } yield newType
  case IdenExpr(_, _) => Left(None)
  case RefExpr(expr, _) => for {
    exprType <- trivialTypeof(container, expr)
  } yield RefDatatype(exprType, false)
  case ValExpr(expr, _) => for {
    exprType <- trivialTypeof(container, expr)
    newType <- exprType match {
      case RefDatatype(datatype, _) => Right(datatype)
      case datatype => Left(Some(Error.semantic(s"Expected the expression to be invoked to be of a reference type, found expression of type '$datatype'", expr.range)))
    }
  } yield newType
  case IntExpr(_, _) => Right(IntDatatype)
  case BoolExpr(_, _) => Right(BoolDatatype)
  case TupleExpr(elements, _) => elements.map(e => trivialTypeof(container, e)).extract.mapBoth(reduceOptionalErrors, datatypes => TupleDatatype(datatypes, false))
  case BlockExpr(_, expr, _) => trivialTypeof(container, expr)
  case UnitExpr(_) => Right(UnitDatatype)
  case DotExpr(expr, iden, _) => ???
  case FunExpr(parameterPatterns, returnTypeExpr, expr, _) => for {
    params <- parameterPatterns.map(p => trivialTypeOf(container, p)).extract.mapLeft(reduceOptionalErrors)
    returnType <- (for {
      typeExpr <- returnTypeExpr.toRight(None)
      returnType <- resolveTypeExpr(container, typeExpr).mapLeft(Some.apply)
    } yield returnType).orElse(trivialTypeof(container, expr))
  } yield FunDatatype(params, returnType, false)
  case IfExpr(_, ifBlock, elseBlock, range) => for {
    ifType <- trivialTypeof(container, ifBlock)
    elseType <- trivialTypeof(container, elseBlock)
    result <- (ifType, elseType) match {
      case (UnitDatatype, UnitDatatype) => Right(UnitDatatype)
      case (_, UnitDatatype) => Right(ifType)
      case (UnitDatatype, _) => Right(elseType)
      case _ if ifType == elseType => Right(ifType)
      case _ =>
        val ifBranch = ErrorComponent(ifBlock.range, Some(s"The if branch is of type '$ifType'"))
        val elseBranch = ErrorComponent(elseBlock.range, Some(s"The else branch is of type '$elseType'"))
        Left(Some(Error(Error.SEMANTIC, range.file, List(ifBranch, elseBranch), Some(s"Both branches of an if statement must return the same value"))))
    }
  } yield result
}

private def visitIncompletePattern(container: ConstAndTypeContainer, pattern: Pattern[Const], datatype: Datatype): Either[CompilerError, Unit] = pattern match {
  case pattern@VarPattern(name, typeExpr, _) => typeExpr match {
    case Some(typeExpr) => for {
      resolvedDatatype <- resolveTypeExpr(container, typeExpr)
      result <- if (datatype.isSubtypeOf(resolvedDatatype)) {
        val const = Const(name, resolvedDatatype)
        pattern.variable = Some(const)
        container.addConst(const)
        Right(())
      } else {
        Left(Error.semantic(s"Type mismatch, variable of type $resolvedDatatype does not match value of type $datatype", typeExpr.range))
      }
    } yield result
    case None =>
      val const = Const(name, datatype)
      pattern.variable = Some(const)
      container.addConst(const)
      Right(())
  }
  case TuplePattern(patterns, range) => datatype match {
    case TupleDatatype(datatypes, _) if patterns.length == datatypes.length => patterns.zip(datatypes).map {
      case (p, d) => visitIncompletePattern(container, p, d)
    }.extract.mapBoth(_.reduce(_ | _), _ => ())
    case _ => Left(Error.semantic("The pattern did not match the return type of the expression", range))
  }
}

private def visitCompletePattern(container: ConstAndTypeContainer, pattern: Pattern[Const]): Either[CompilerError, Unit] = pattern match {
  case pattern@VarPattern(name, typeExpr, _) => for {
    resolvedDatatype <- resolveTypeExpr(container, typeExpr.get)
  } yield {
    val const = Const(name, resolvedDatatype)
    pattern.variable = Some(const)
    container.addConst(const)
    Right(())
  }
  case TuplePattern(elements, _) => elements.map(p => visitCompletePattern(container, p)).extract.mapBoth(_.reduce(_ | _), _ => ())
}

private def resolveConstType(container: ConstAndTypeContainer, constStmt: ConstStmt) = if (constStmt.pattern.incomplete) {
  for {
    datatype <- trivialTypeof(container, constStmt.expr).mapLeft(_.getOrElse(Error.semantic("Constants without complete explicit type must be trivially type inferrable (The expression cannot reference other constants, use explicit type annotations where this is not possible)", constStmt.expr.range)))
    _ <- visitIncompletePattern(container, constStmt.pattern, datatype)
  } yield ()
} else {
  visitCompletePattern(container, constStmt.pattern)
}

private def resolveConstTypes(container: ConstAndTypeContainer): Either[CompilerError, Unit] = container.constStmts.map(constStmt => resolveConstType(container, constStmt)).extract.mapBoth(_.reduce(_ | _), _ => ())

private def resolveConstValue(container: ConstAndTypeContainer, constStmt: ConstStmt): Either[CompilerError, Unit] = ???

private def resolveConstValues(container: ConstAndTypeContainer): Either[CompilerError, Unit] = container.constStmts.map(constStmt => resolveConstType(container, constStmt)).extract.mapBoth(_.reduce(_ | _), _ => ())

private def collectModuleVars(module: Module): Unit = {
  module.varStmts.foreach { varStmt =>
    varStmt.pattern.foreach { case VarPattern(name, typeExpr, range) =>
      ()
    }
  }

  Right(())
}

def semanticAnalysis(module: Module): Either[CompilerError, Unit] = {
  resolveTypes(module)
  
  
  
  Right(())
}
