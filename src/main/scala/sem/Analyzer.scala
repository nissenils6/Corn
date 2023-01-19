package sem

import core.*
import sem.ConstUnit.datatype
import syn.*

import scala.annotation.tailrec
import scala.collection.mutable

private def collectContainerConsts(container: Container): Unit = for {
  constStmt <- container.constStmts
  pattern <- constStmt.pattern
} {
  val const = Const(pattern.name, Some(constStmt))
  container.addConst(const)
  pattern.variable = Some(const)
}

private def collectModuleVars(module: Module): Unit = for {
  varStmt <- module.varStmts
  pattern <- varStmt.pattern
} {
  val variable = Var(pattern.name, Some(varStmt))
  module.addVar(variable)
  pattern.variable = Some(variable)
}

private def computePatternType[T <: AnyVar](container: Container, pattern: Pattern[T]): Either[CompilerError, Datatype] = pattern match {
  case VarPattern(_, Some(typeExpr), _) => resolveTypeExpr(container, typeExpr)
  case VarPattern(_, None, range) => Left(Error.semantic("Explicit type annotation expected here", range))
  case TuplePattern(elements, _) => elements.map(p => computePatternType(container, p)).extract.mapBoth(_.reduce(_ | _), datatypes => TupleDatatype(datatypes, true))
}

private def gatherParameterVars(container: Container, pattern: Pattern[Var], locals: mutable.Map[String, Var]): Either[CompilerError, Unit] = pattern match {
  case pattern: VarPattern[Var] => for {
    datatype <- resolveTypeExpr(container, pattern.typeExpr.get)
  } yield {
    val variable = new Var(pattern.name, None)
    variable.datatype = Some(datatype)
    pattern.variable = Some(variable)
    locals(pattern.name) = variable
    ()
  }
  case TuplePattern(elements, _) => elements.map(p => gatherParameterVars(container, p, locals)).extract.mapBoth(_.reduce(_ | _), _ => ())
}

private def typeCheckRegConstStmt(container: Container, stmt: Stmt, locals: mutable.Map[String, Var], visited: List[(AnyVarStmt, FilePosRange)]): Either[CompilerError, Unit] = stmt match {
  case ExprStmt(expr, range) => ???
  case AssignVarStmt(iden, expr, range) => ???
  case AssignRefStmt(refExpr, expr, range) => ???
  case LocalVarStmt(pattern, expr, _, range) => ???
}

@tailrec
private def lookupConstIden(container: Container, iden: String, range: FilePosRange): Either[CompilerError, Option[Const]] = (container.consts.get(iden), container.parent) match {
  case (Some(List(const)), _) => Right(Some(const))
  case (None, Some(parent)) => lookupConstIden(parent, iden, range)
  case (Some(Nil), Some(parent)) => lookupConstIden(parent, iden, range)
  case (Some(consts), _) =>
    val components = ErrorComponent(range, Some("Identifier that failed to be resolved")) :: consts.map(_.stmt).map {
      case Some(stmt) => ErrorComponent(stmt.nameRange, Some("One alternative is defined here"))
      case None => ErrorComponent(container.module.get.file.lastRange, Some("One alternative is defined as a builtin"))
    }
    Left(Error(Error.SEMANTIC, range.file, components, Some("Multiple alternatives when resolving a constant")))
  case _ => Right(None)
}

private def lookupVarIden(module: Module, iden: String, range: FilePosRange): Either[CompilerError, Option[Var]] = module.vars.get(iden) match {
  case Some(List(variable)) => Right(Some(variable))
  case Some(vars) =>
    val components = ErrorComponent(range, Some("Identifier that failed to be resolved")) :: vars.map(_.stmt).map {
      case Some(stmt) => ErrorComponent(stmt.nameRange, Some("One alternative is defined here"))
      case None => ErrorComponent(module.file.lastRange, Some("One alternative is defined as a builtin"))
    }
    Left(Error(Error.SEMANTIC, range.file, components, Some("Multiple alternatives when resolving a constant")))
  case _ => Right(None)
}

private def typeCheckConstExpr(expr: Expr, locals: mutable.Map[String, Var], visited: List[(AnyVarStmt, FilePosRange)]): Either[CompilerError, Datatype] = expr match {
  case CallExpr(function, args, range) => for {
    functionType <- typeCheckConstExpr(function, locals, visited)
    argTypes <- args.map(a => typeCheckConstExpr(a, locals, visited)).extract.mapLeft(_.reduce(_ | _))
    result <- functionType match {
      case FunDatatype(params, returnType, _) if argTypes.length == params.length && argTypes.zip(params).forall { case (a, p) => a.isSubtypeOf(p) } => Right(returnType.withMut(true))
      case FunDatatype(params, _, _) => Left(Error.semantic(s"The arguments '${argTypes.mkString(", ")}' does not match the parameters '${params.mkString(", ")}'", range))
      case _ => Left(Error.semantic(s"Expected the expression to be invoked to of function type, found expression of type '$functionType'", function.range))
    }
  } yield result
  case idenExpr@IdenExpr(iden, range) => idenExpr.variable match {
    case Some(const: Const) => for {
      _ <- const.stmt match {
        case Some(constStmt) => typeCheckConstStmt(constStmt, (constStmt, range) :: visited)
        case None => Right(())
      }
    } yield const.datatype.get
    case Some(variable: Var) => Right(variable.datatype.get)
    case None => locals.get(iden) match {
      case Some(variable) =>
        idenExpr.variable = Some(variable)
        Right(variable.datatype.get)
      case None => for {
        constOption <- lookupConstIden(expr.parent.get, iden, range)
        const <- constOption.toRight(Error.semantic(s"Could not resolve '$iden' as a constant", range))
        datatype <- const.datatype match {
          case Some(datatype) => Right(datatype)
          case None => for {
            _ <- typeCheckConstStmt(const.stmt.get, (const.stmt.get, range) :: visited)
          } yield const.datatype.get
        }
      } yield {
        idenExpr.variable = Some(const)
        datatype
      }
    }
  }
  case RefExpr(_, range) => Left(Error.semantic("References are not allowed in compile time contexts, yet", range))
  case ValExpr(_, range) => Left(Error.semantic("References are not allowed in compile time contexts, yet", range))
  case IntExpr(_, _) => Right(MutIntDatatype)
  case BoolExpr(_, _) => Right(MutBoolDatatype)
  case TupleExpr(elements, _) => elements.map(e => typeCheckConstExpr(e, locals, visited)).extract.mapBoth(_.reduce(_ | _), datatypes => TupleDatatype(datatypes, true))
  case blockExpr@BlockExpr(_, expr, _) =>
    val newLocals = locals.clone()
    collectContainerConsts(blockExpr)
    for {
      _ <- resolveTypes(blockExpr)
      _ <- blockExpr.regStmts.map(s => typeCheckRegConstStmt(blockExpr, s, newLocals, visited)).extract.mapBoth(_.reduce(_ | _), _ => ())
      datatype <- typeCheckConstExpr(expr, newLocals, visited)
    } yield datatype
  case UnitExpr(range) => Right(UnitDatatype)
  case DotExpr(expr, iden, range) => ???
  case funExpr@FunExpr(parameterPatterns, returnTypeExpr, bodyExpr, _) => funExpr.signature match {
    case Some(signature) => Right(signature)
    case None => for {
      paramTypes <- parameterPatterns.map(p => computePatternType(funExpr.parent.get, p)).extract.mapLeft(_.reduce(_ | _))
      returnType <- returnTypeExpr match {
        case Some(returnTypeExpr) => resolveTypeExpr(funExpr.parent.get, returnTypeExpr)
        case None =>
          val newLocals: mutable.Map[String, Var] = mutable.Map.empty
          for {
            _ <- parameterPatterns.map(p => gatherParameterVars(funExpr.parent.get, p, newLocals)).extract.mapBoth(_.reduce(_ | _), _ => ())
            returnType <- typeCheckExpr(bodyExpr, newLocals, visited)
          } yield {
            funExpr.bodyTypeChecked = true
            returnType
          }
      }
    } yield {
      val signature = FunDatatype(paramTypes, returnType, true)
      funExpr.signature = Some(signature)
      signature
    }
  }
  case IfExpr(condition, ifBlock, elseBlock, _) => for {
    conditionType <- typeCheckConstExpr(condition, locals, visited)
    _ <- conditionType match {
      case BoolDatatype | MutBoolDatatype => Right(())
      case _ => Left(Error.semantic(s"Expected the condition to be of boolean type, found expression of type '$conditionType'", condition.range))
    }
    ifType <- typeCheckConstExpr(ifBlock, locals, visited)
    elseType <- typeCheckConstExpr(elseBlock, locals, visited)
    result <- (ifType, elseType) match {
      case (UnitDatatype, UnitDatatype) => Right(UnitDatatype)
      case (UnitDatatype, datatype) => Right(datatype)
      case (datatype, UnitDatatype) => Right(datatype)
      case (subType, superType) if subType.isSubtypeOf(superType) => Right(superType)
      case (superType, subType) if subType.isSubtypeOf(superType) => Right(superType)
      case (_, _) =>
        val ifComponent = ErrorComponent(ifBlock.range, Some(s"The if branch is of type '$ifType'"))
        val elseComponent = ErrorComponent(elseBlock.range, Some(s"The else branch is of type '$elseType'"))
        Left(Error(Error.SEMANTIC, condition.range.file, List(ifComponent, elseComponent), Some("If expressions whose two branches are expressions of unrelated types are not allowed")))
    }
  } yield result
}

private def typeCheckConstPattern(container: Container, constStmt: ConstStmt, pattern: Pattern[Const], datatype: Datatype): Either[CompilerError, Unit] = pattern match {
  case pattern@VarPattern(_, typeExpr, range) => for {
    typedDatatype <- typeExpr match {
      case Some(typeExpr) => resolveTypeExpr(container, typeExpr).map(Some.apply)
      case None => Right(None)
    }
    _ <- if (typedDatatype.forall(datatype.isSubtypeOf)) {
      pattern.variable.get.datatype = Some(typedDatatype.getOrElse(datatype.withMut(false)))
      Right(())
    } else {
      Left(Error.semantic(s"type '$datatype' does not match type '$typedDatatype'", range))
    }
  } yield ()
  case TuplePattern(patterns, range) => datatype match {
    case TupleDatatype(datatypes, _) if patterns.length == datatypes.length => patterns.zip(datatypes).map { case (p, d) => typeCheckConstPattern(container, constStmt, p, d) }.extract.mapBoth(_.reduce(_ | _), _ => ())
    case TupleDatatype(datatypes, _) => Left(Error.semantic(s"Expected the expression to be of tuple type with ${patterns.length} elements to match a tuple pattern, found expression of tuple type with '${datatypes.length} elements'", range))
    case _ => Left(Error.semantic(s"Expected the expression to be of tuple type to match a tuple pattern, found expression of type '$datatype'", range))
  }
}

private def typeCheckConstPattern(container: Container, constStmt: ConstStmt, pattern: Pattern[Const]): Either[CompilerError, Unit] = pattern match {
  case pattern@VarPattern(_, typeExpr, _) => for {
    typedDatatype <- resolveTypeExpr(container, typeExpr.get)
  } yield {
    pattern.variable.get.datatype = Some(typedDatatype)
    Right(())
  }
  case TuplePattern(patterns, _) => patterns.map(p => typeCheckConstPattern(container, constStmt, p)).extract.mapBoth(_.reduce(_ | _), _ => ())
}

private def typeCheckConstStmt(constStmt: ConstStmt, visited: List[(AnyVarStmt, FilePosRange)]): Either[CompilerError, Unit] = if (!constStmt.bodyTypeChecked) {
  if (constStmt.pattern.incomplete) for {
    datatype <- typeCheckConstExpr(constStmt.expr, mutable.Map.empty, visited)
    _ <- typeCheckConstPattern(constStmt.expr.parent.get, constStmt, constStmt.pattern, datatype)
  } yield {
    constStmt.bodyTypeChecked = true
  } else for {
    _ <- typeCheckConstPattern(constStmt.expr.parent.get, constStmt, constStmt.pattern)
  } yield {
    constStmt.bodyTypeChecked = true
  }
} else Right(())

private def typeCheckVarStmt(globalVarStmt: GlobalVarStmt, visited: List[(AnyVarStmt, FilePosRange)]): Either[CompilerError, Unit] = ???

private def typeCheckPattern(container: Container, varStmt: VarStmt, pattern: Pattern[Var], datatype: Datatype, locals: mutable.Map[String, Var]): Either[CompilerError, Unit] = pattern match {
  case pattern@VarPattern(name, typeExpr, range) => for {
    typedDatatype <- typeExpr match {
      case Some(typeExpr) => resolveTypeExpr(container, typeExpr).map(Some.apply)
      case None => Right(None)
    }
    _ <- if (typedDatatype.forall(datatype.isSubtypeOf)) {
      val variable = Var(name, Some(varStmt))
      variable.datatype = Some(typedDatatype.getOrElse(datatype.withMut(false)))
      pattern.variable = Some(variable)
      locals(name) = variable
      Right(())
    } else {
      Left(Error.semantic(s"type '$datatype' does not match type '$typedDatatype'", range))
    }
  } yield ()
  case TuplePattern(patterns, range) => datatype match {
    case TupleDatatype(datatypes, _) if patterns.length == datatypes.length => patterns.zip(datatypes).map { case (p, d) => typeCheckPattern(container, varStmt, p, d, locals) }.extract.mapBoth(_.reduce(_ | _), _ => ())
    case TupleDatatype(datatypes, _) => Left(Error.semantic(s"Expected the expression to be of tuple type with ${patterns.length} elements to match a tuple pattern, found expression of tuple type with '${datatypes.length} elements'", range))
    case _ => Left(Error.semantic(s"Expected the expression to be of tuple type to match a tuple pattern, found expression of type '$datatype'", range))
  }
}

private def typeCheckRegStmt(container: Container, stmt: Stmt, locals: mutable.Map[String, Var], visited: List[(AnyVarStmt, FilePosRange)]): Either[CompilerError, Unit] = stmt match {
  case ExprStmt(expr, range) => for {
    _ <- typeCheckExpr(expr, locals, visited)
  } yield ()
  case AssignVarStmt(iden, expr, range) => ???
  case AssignRefStmt(refExpr, expr, range) => ???
  case varStmt@LocalVarStmt(pattern, expr, _, range) => for {
    datatype <- typeCheckExpr(expr, locals, visited)
    _ <- typeCheckPattern(container, varStmt, pattern, datatype, locals)
  } yield ()
}

private def typeCheckExpr(expr: Expr, locals: mutable.Map[String, Var], visited: List[(AnyVarStmt, FilePosRange)]): Either[CompilerError, Datatype] = expr match {
  case CallExpr(function, args, range) => for {
    functionType <- typeCheckExpr(function, locals, visited)
    argTypes <- args.map(a => typeCheckExpr(a, locals, visited)).extract.mapLeft(_.reduce(_ | _))
    result <- functionType match {
      case FunDatatype(params, returnType, _) if argTypes.length == params.length && argTypes.zip(params).forall { case (a, p) => a.isSubtypeOf(p) } => Right(returnType.withMut(true))
      case FunDatatype(params, _, _) => Left(Error.semantic(s"The arguments '${argTypes.mkString(", ")}' does not match the parameters '${params.mkString(", ")}'", range))
      case _ => Left(Error.semantic(s"Expected the expression to be invoked to of function type, found expression of type '$functionType'", function.range))
    }
  } yield result
  case idenExpr@IdenExpr(iden, range) => idenExpr.variable match {
    case Some(const: Const) => for {
      _ <- const.stmt match {
        case Some(constStmt) => typeCheckConstStmt(constStmt, (constStmt, range) :: visited)
        case None => Right(())
      }
    } yield const.datatype.get
    case Some(variable: Var) => Right(variable.datatype.get)
    case None => locals.get(iden) match {
      case Some(variable) =>
        idenExpr.variable = Some(variable)
        Right(variable.datatype.get)
      case None => for {
        constOption <- lookupConstIden(expr.parent.get, iden, range)
        varOption <- lookupVarIden(idenExpr.parent.get.module.get, iden, range)
        variable <- constOption.orElse(varOption).toRight(Error.semantic(s"Could not resolve '$iden' as a variable or constant", range))
        datatype <- variable.datatype match {
          case Some(datatype) => Right(datatype)
          case None => for {
            _ <- variable match {
              case const: Const => typeCheckConstStmt(const.stmt.get, (const.stmt.get, range) :: visited)
              case variable: Var => typeCheckVarStmt(variable.stmt.get.asInstanceOf[GlobalVarStmt], (variable.stmt.get, range) :: visited)
            }
          } yield variable.datatype.get
        }
      } yield {
        idenExpr.variable = Some(variable)
        datatype
      }
    }
  }
  case RefExpr(_, range) => ???
  case ValExpr(_, range) => ???
  case IntExpr(_, _) => Right(MutIntDatatype)
  case BoolExpr(_, _) => Right(MutBoolDatatype)
  case TupleExpr(elements, _) => ???
  case blockExpr@BlockExpr(_, expr, _) =>
    val newLocals = locals.clone()
    collectContainerConsts(blockExpr)
    for {
      _ <- resolveTypes(blockExpr)
      _ <- blockExpr.regStmts.map(s => typeCheckRegStmt(blockExpr, s, newLocals, visited)).extract.mapBoth(_.reduce(_ | _), _ => ())
      datatype <- typeCheckExpr(expr, newLocals, visited)
    } yield datatype
  case UnitExpr(range) => Right(UnitDatatype)
  case DotExpr(expr, iden, range) => ???
  case funExpr@FunExpr(parameterPatterns, returnTypeExpr, bodyExpr, _) => ???
  case IfExpr(condition, ifBlock, elseBlock, _) => ???
}

def semanticAnalysis(module: Module): Either[CompilerError, Unit] = for {
  _ <- resolveTypes(module)
  _ = collectContainerConsts(module)
  _ = collectModuleVars(module)
  _ <- module.constStmts.map(s => typeCheckConstStmt(s, List.empty)).extract.mapBoth(_.reduce(_ | _), _ => ())
} yield ()
