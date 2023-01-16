package sem

import core.*
import sem.ConstUnit.datatype
import syn.*

import scala.annotation.tailrec
import scala.collection.mutable

private def setParentRecursive(module: Module, parent: Container)(expr: Expr): Unit = {
  expr.parent = Some(module)

  expr match {
    case CallExpr(function, args, _) => (function :: args).foreach(setParentRecursive(module, parent))
    case IdenExpr(_, _) => ()
    case RefExpr(expr, _) => setParentRecursive(module, parent)(expr)
    case ValExpr(expr, _) => setParentRecursive(module, parent)(expr)
    case IntExpr(_, _) => ()
    case BoolExpr(_, _) => ()
    case TupleExpr(elements, _) => elements.foreach(setParentRecursive(module, parent))
    case newParent@BlockExpr(stmts, expr, _) =>
      newParent.module = Some(module)
      newParent.parent = Some(parent)
      stmts.foreach {
        case ExprStmt(expr, _) => setParentRecursive(module, newParent)(expr)
        case AssignVarStmt(_, expr, _) => setParentRecursive(module, newParent)(expr)
        case AssignRefStmt(refExpr, expr, _) =>
          setParentRecursive(module, newParent)(refExpr)
          setParentRecursive(module, newParent)(expr)
        case stmt@LocalVarStmt(_, expr, _, _) =>
          setParentRecursive(module, newParent)(expr)
          stmt.parent = Some(newParent)
        case stmt@LocalConstStmt(_, expr, _, _) =>
          setParentRecursive(module, newParent)(expr)
          stmt.parent = Some(newParent)
        case LocalTypeStmt(_, _, _, _) => ()
      }
      setParentRecursive(module, newParent)(expr)
    case UnitExpr(_) => ()
    case DotExpr(expr, _, _) => setParentRecursive(module, parent)(expr)
    case FunExpr(_, _, expr, _) => setParentRecursive(module, parent)(expr)
    case IfExpr(condition, ifBlock, elseBlock, _) =>
      setParentRecursive(module, parent)(condition)
      setParentRecursive(module, parent)(ifBlock)
      setParentRecursive(module, parent)(elseBlock)
  }
}

def resolveScopeParents(module: Module): Unit = {
  module.varStmts.foreach(_.parent = Some(module))
  module.constStmts.foreach(_.parent = Some(module))

  module.varStmts.map(_.expr).foreach(setParentRecursive(module, module))
  module.constStmts.map(_.expr).foreach(setParentRecursive(module, module))
}

private def reduceOptionalErrors(errors: List[Option[CompilerError]]): Option[CompilerError] = errors.flatten match {
  case Nil => None
  case errors => Some(errors.reduce(_ | _))
}

private def resolveTypeExprRecursive(container: Container, typeExpr: TypeExpr, visited: List[(TypeStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeExpr match {
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

private def resolveTypeVar(container: Container, typeVar: TypeVar, causeRange: FilePosRange, visited: List[(TypeStmt, FilePosRange)]): Either[CompilerError, Datatype] = typeVar.value match {
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

private def resolveTypes(container: Container): Either[CompilerError, Unit] = for {
  _ <- container.typeStmts.map { typeStmt =>
    container.addType(typeStmt.typeVar)
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
  _ <- container.typeStmts.map { typeStmt =>
    resolveTypeVar(container, typeStmt.typeVar, typeStmt.nameRange, List())
  }.extract.mapBoth(_.reduce(_ | _), _ => ())
} yield ()

private def resolveTypeExpr(container: Container, typeExpr: TypeExpr): Either[CompilerError, Datatype] = typeExpr match {
  case UnitTypeExpr(_) => Right(UnitDatatype)
  case IdenTypeExpr(iden, range) => container.types.get(iden).map(_.value.get).toRight(Error.semantic(s"Could not resolve '$iden' as a type", range))
  case TupleTypeExpr(elements, _) => for {
    datatypes <- elements.map(e => resolveTypeExpr(container, e)).extract.mapLeft(_.reduce(_ | _))
  } yield TupleDatatype(datatypes, false)
  case MutTypeExpr(typeExpr, range) => ???
  case RefTypeExpr(typeExpr, range) => ???
  case FunTypeExpr(params, returnType, range) => ???
}

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

private def gatherParameterVars(pattern: Pattern[Var], locals: mutable.Map[String, Var]): Unit = pattern match {
  case pattern: VarPattern[Var] => locals(pattern.name) = pattern.variable.get
  case TuplePattern(elements, _) => elements.foreach(p => gatherParameterVars(p, locals))
}

private def typeCheckRegConstStmt(container: Container, stmt: Stmt, locals: mutable.Map[String, Var], visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Unit] = stmt match {
  case ExprStmt(expr, range) => ???
  case AssignVarStmt(iden, expr, range) => ???
  case AssignRefStmt(refExpr, expr, range) => ???
  case LocalVarStmt(pattern, expr, _, range) => ???
}

@tailrec
private def lookupConstIden(container: Container, iden: String, range: FilePosRange): Either[CompilerError, Const] = (container.consts.get(iden), container.parent) match {
  case (Some(List(const)), _) => Right(const)
  case (None, Some(parent)) => lookupConstIden(parent, iden, range)
  case (Some(Nil), Some(parent)) => lookupConstIden(parent, iden, range)
  case (Some(consts), _) =>
    val components = ErrorComponent(range, Some("Identifier that failed to be resolved")) :: consts.map(_.stmt).map {
      case Some(stmt) => ErrorComponent(stmt.nameRange, Some("One alternative is defined here"))
      case None => ErrorComponent(container.module.get.file.lastRange, Some("One alternative is defined as a builtin"))
    }
    Left(Error(Error.SEMANTIC, range.file, components, Some("Multiple alternatives when resolving a constant")))
  case _ => Left(Error.semantic(s"Could not resolve '$iden' as a constant", range))
}

private def typeCheckConstExpr(expr: Expr, locals: mutable.Map[String, Var], visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Datatype] = expr match {
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
        const <- lookupConstIden(expr.parent.get, iden, range)
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
        case None => for {
          returnType <- {
            val newLocals: mutable.Map[String, Var] = mutable.Map.empty
            parameterPatterns.foreach(p => gatherParameterVars(p, newLocals))
            typeCheckExpr(bodyExpr, newLocals, visited)
          }
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

private def evaluateAssignment(pattern: Pattern[Var], constVal: ConstVal, locals: mutable.Map[Var, ConstVal]): Unit = (pattern, constVal) match {
  case (varPattern: VarPattern[Var], _) => locals(varPattern.variable.get) = constVal
  case (TuplePattern(patterns, _), ConstTuple(values)) => patterns.zip(values).foreach { case (p, v) => evaluateAssignment(p, v, locals) }
}

private def evaluateConstExpr(expr: Expr, locals: mutable.Map[Var, ConstVal]): ConstVal = expr match {
  case CallExpr(function, args, _) =>
    val ConstFun(fun) = evaluateConstExpr(function, locals)
    val argVals = args.map(a => evaluateConstExpr(a, locals))
    fun match {
      case BuiltinFun(_, _, eval, _) => eval.get(argVals)
      case FunExpr(parameterPatterns, _, expr, _) =>
        val newLocals: mutable.Map[Var, ConstVal] = mutable.Map.empty
        parameterPatterns.zip(argVals).foreach { case (p, a) => evaluateAssignment(p, a, newLocals) }
        evaluateConstExpr(expr, locals)
    }
  case IdenExpr(iden, range) => ???
  case RefExpr(_, range) => assert(false, "References are not allowed during compile time execution, yet")
  case ValExpr(_, range) => assert(false, "References are not allowed during compile time execution, yet")
  case IntExpr(int, _) => ConstInt(int)
  case BoolExpr(bool, _) => ConstBool(bool)
  case TupleExpr(elements, _) => ConstTuple(elements.map(e => evaluateConstExpr(e, locals)))
  case blockExpr@BlockExpr(_, expr, _) =>
    blockExpr.regStmts.foreach {
      case ExprStmt(expr, _) => evaluateConstExpr(expr, locals)
      case assignment@AssignVarStmt(_, expr, _) => locals(assignment.variable.get) = evaluateConstExpr(expr, locals)
      case AssignRefStmt(_, _, _) => assert(false, "References are not allowed during compile time execution, yet")
      case LocalVarStmt(pattern, expr, _, _) => evaluateAssignment(pattern, evaluateConstExpr(expr, locals), locals)
    }
    evaluateConstExpr(expr, locals)
  case UnitExpr(_) => ConstUnit
  case DotExpr(expr, iden, range) => ???
  case funExpr: FunExpr => ConstFun(funExpr)
  case IfExpr(condition, ifBlock, elseBlock, _) => evaluateConstExpr(condition, locals) match {
    case ConstBool(true) => evaluateConstExpr(ifBlock, locals)
    case ConstBool(true) => evaluateConstExpr(elseBlock, locals)
  }
}

private def typeCheckConstPattern(container: Container, pattern: Pattern[Const], datatype: Datatype): Either[CompilerError, Unit] = pattern match {
  case pattern: VarPattern[Const] => pattern.typeExpr match {
    case Some(typeExpr) => for {
      typedDatatype <- resolveTypeExpr(container, typeExpr)
      _ <- if (typedDatatype.isSubtypeOf(datatype)) {
        pattern.variable.get.datatype = Some(typedDatatype)
        Right(())
      } else {
        Left(Error.semantic(s"Type mismatch: Pattern is of type '$typedDatatype', but the matching expression is of type '$datatype'", null))
      }
    } yield ()
    case None =>
      pattern.variable.get.datatype = Some(datatype)
      Right(())
  }
  case TuplePattern(patterns, range) => datatype match {
    case TupleDatatype(datatypes, _) if patterns.length == datatypes.length => patterns.zip(datatypes).map { case (p, d) => typeCheckConstPattern(container, p, d) }.extract.mapBoth(_.reduce(_ | _), _ => ())
    case TupleDatatype(datatypes, _) => Left(Error.semantic(s"Expected the expression to be of tuple type with ${patterns.length} elements to match a tuple pattern, found expression of tuple type with '${datatypes.length} elements'", range))
    case _ => Left(Error.semantic(s"Expected the expression to be of tuple type to match a tuple pattern, found expression of type '$datatype'", range))
  }
}

private def typeCheckConstPattern(container: Container, pattern: Pattern[Const]): Either[CompilerError, Unit] = pattern match {
  case pattern: VarPattern[Const] => for {
    typedDatatype <- resolveTypeExpr(container, pattern.typeExpr.get)
  } yield {
    pattern.variable.get.datatype = Some(typedDatatype)
  }
  case TuplePattern(patterns, _) => patterns.map(p => typeCheckConstPattern(container, p)).extract.mapBoth(_.reduce(_ | _), _ => ())
}

private def typeCheckConstStmt(constStmt: ConstStmt, visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Unit] = if (!constStmt.bodyTypeChecked) {
  if (constStmt.pattern.incomplete) for {
    datatype <- typeCheckConstExpr(constStmt.expr, mutable.Map.empty, visited)
    _ <- typeCheckConstPattern(constStmt.expr.parent.get, constStmt.pattern, datatype)
  } yield {
    constStmt.bodyTypeChecked = true
  } else for {
    _ <- typeCheckConstPattern(constStmt.expr.parent.get, constStmt.pattern)
  } yield {
    constStmt.bodyTypeChecked = true
  }
} else Right(())

private def visitAssignConstPattern(pattern: Pattern[Const], constVal: ConstVal): Either[CompilerError, Unit] = pattern match {
  case pattern: VarPattern[Const] =>
    pattern.variable.get.value = Some(constVal)
    Right(())
  case TuplePattern(patterns, _) =>
    val ConstTuple(constVals) = constVal
    patterns.zip(constVals).map { case (p, c) => visitAssignConstPattern(p, c) }.extract.mapBoth(_.reduce(_ | _), _ => ())
}

private def evaluateConstStmt(container: Container, constStmt: ConstStmt): Either[CompilerError, Unit] = ??? /*if (constStmt.value.isEmpty) for {
  constVal <- evaluateConstExpr(constStmt.expr, mutable.Map.empty)
  _ <- visitAssignConstPattern(container, constStmt.pattern, constVal)
} yield ()*/

private def typeCheckPattern(container: Container, varStmt: VarStmt, pattern: Pattern[Var], datatype: Datatype, locals: mutable.Map[String, Var]): Either[CompilerError, Unit] = pattern match {
  case pattern@VarPattern(name, typeExpr, range) => for {
    typedDatatype <- resolveTypeExpr(container, typeExpr)
    _ <- if (datatype.isSubtypeOf(typedDatatype)) {
      val variable = Var(name, Some(varStmt))
      pattern.variable = Some(variable)
      locals(name) = variable
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

private def typeCheckRegStmt(container: Container, stmt: Stmt, locals: mutable.Map[String, Var], visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Unit] = stmt match {
  case ExprStmt(expr, range) => ???
  case AssignVarStmt(iden, expr, range) => ???
  case AssignRefStmt(refExpr, expr, range) => ???
  case varStmt@LocalVarStmt(pattern, expr, _, range) => for {
    datatype <- typeCheckExpr(expr, locals, visited)
    _ <- typeCheckPattern(container, varStmt, pattern, datatype, locals)
  } yield ()
}

private def typeCheckExpr(expr: Expr, locals: mutable.Map[String, Var], visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Datatype] = expr match {
  case CallExpr(function, args, range) => ???
  case idenExpr@IdenExpr(iden, range) => ???
  case RefExpr(_, range) => ???
  case ValExpr(_, range) => ???
  case IntExpr(_, _) => ???
  case BoolExpr(_, _) => ???
  case TupleExpr(elements, _) => ???
  case blockExpr@BlockExpr(_, expr, _) =>
    val newLocals = locals.clone()
    for {
      _ <- resolveTypes(blockExpr)
      _ <- blockExpr.regStmts.map(s => typeCheckRegStmt(blockExpr, s, newLocals, visited)).extract.mapBoth(_.reduce(_ | _), _ => ())
      datatype <- typeCheckExpr(expr, newLocals, visited)
    } yield datatype
  case UnitExpr(range) => ???
  case DotExpr(expr, iden, range) => ???
  case funExpr@FunExpr(parameterPatterns, returnTypeExpr, bodyExpr, _) => ???
  case IfExpr(condition, ifBlock, elseBlock, _) => ???
}

def semanticAnalysis(module: Module): Either[CompilerError, Unit] = for {
  _ <- resolveTypes(module)
  _ <- module.constStmts.map(s => typeCheckConstStmt(s, List.empty)).extract.mapBoth(_.reduce(_ | _), _ => ())
} yield ()
