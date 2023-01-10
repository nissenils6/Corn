package sem

import core.*
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
        case stmt@LocalVarStmt(_, expr, _) =>
          setParentRecursive(module, newParent)(expr)
          stmt.parent = Some(newParent)
        case stmt@LocalConstStmt(_, expr, _) =>
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
  module.addConst(const)
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
  case VarPattern(_, typeExpr, _) => resolveTypeExpr(container, typeExpr)
  case TuplePattern(elements, _) => elements.map(p => computePatternType(container, p)).extract.mapBoth(_.reduce(_ | _), datatypes => TupleDatatype(datatypes, true))
}

private def typeCheckRegConstStmt(container: Container, stmt: Stmt, locals: mutable.Map[Var, ConstVal], visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Unit] = stmt match {
  case ExprStmt(expr: Expr, range: FilePosRange) => ???
  case AssignVarStmt(iden: String, expr: Expr, range: FilePosRange) => ???
  case AssignRefStmt(refExpr: Expr, expr: Expr, range: FilePosRange) => ???
  case LocalVarStmt(pattern: Pattern[Var], expr: Expr, range: FilePosRange) => ???
}

@tailrec
private def lookupConstIden(container: Container, iden: String, range: FilePosRange): Either[CompilerError, Const] = (container.consts.get(iden), container.parent) match {
  case (Some(List(const)), _) => Right((const, container))
  case (None, Some(parent)) => lookupConstIden(parent, iden, range)
  case (Some(Nil), Some(parent)) => lookupConstIden(parent, iden, range)
  case (Some(consts), _) =>
    val components = ErrorComponent(range, Some("Identifier that failed to be resolved")) :: consts.map(_.stmt).map {
      case Some(stmt) => ErrorComponent(stmt.nameRange, Some("One alternative is defined here"))
      case None => ErrorComponent(container.module.file.lastRange, Some("One alternative is defined as a builtin"))
    }
    Left(Error(Error.SEMANTIC, range.file, components, Some("Multiple alternatives when resolving a constant")))
  case _ => Left(Error.semantic(s"Could not resolve '$iden' as a constant", range))
}

private def typeCheckConstExpr(expr: Expr, locals: mutable.Map[String, Var], visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Datatype] = expr match {
  // TODO: Implement overloading
  // case CallExpr(IdenExpr(iden, idenRange), args, range) => ???
  case CallExpr(function, args, range) => for {
    functionType <- typeCheckConstExpr(container, function, visited)
    argTypes <- args.map(a => typeCheckConstExpr(container, a, visited)).extract.mapLeft(_.reduce(_ | _))
    result <- functionType match {
      case FunDatatype(params, returnType, _) if argTypes.length == params.length && argTypes.zip(params).forall { case (a, p) => a.isSubtypeOf(p) } => Right(returnType.withMut(true))
      case FunDatatype(params, _, _) => Left(Error.semantic(s"The arguments '${argTypes.mkString(", ")}' does not match the parameters '${params.mkString(", ")}'", range))
      case _ => Left(Error.semantic(s"Expected the expression to be invoked to of function type, found expression of type '$functionType'"))
    }
  } yield result
  case idenExpr@IdenExpr(iden, range) => idenExpr.variable match {
    case Some(const: Const) => Right(const.datatype.get)
    case Some(variable: Var) => ???
    case None => locals.get(iden) match {
      case Some(variable) =>
        idenExpr.variable = Some(variable)
        // TODO: We are inside a block, make sure that datatypes have already been computed for already declared variables
        variable.datatype.get
      case None => for {
        const <- lookupConstIden(container, iden, range)
        datatype <- const.datatype match {
          case Some(datatype) => Right(datatype)
          case None => resolveConstStmt(const.stmt.get.parent, const.stmt.get, (const.stmt.get, range) :: visited)
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
  case TupleExpr(elements, _) => elements.map(e => typeCheckConstExpr(container, e, visited)).extract.mapBoth(_.reduce(_ | _), datatypes => TupleDatatype(datatypes, true))
  case blockExpr@BlockExpr(stmts, expr, _) => for {
    _ <- resolveTypes(blockExpr)
    _ <- resolveConsts(blockExpr, visited)
    _ <- blockExpr.regStmts.map(s => typeCheckRegConstStmt(blockExpr, s, locals, visited)).extract.mapBoth(_.reduce(_ | _), _ => ())
    datatype <- typeCheckConstExpr(blockExpr, expr, locals, visited)
  } yield datatype
  case UnitExpr(range) => Right(UnitDatatype)
  case DotExpr(expr, iden, range) => ???
  case funExpr@FunExpr(parameterPatterns, returnTypeExpr, bodyExpr, _) => funExpr.signature match {
    case Some(signature) => Right(signature)
    case None => for {
      paramTypes <- parameterPatterns.map(p => computePatternType(container, p)).extract.mapBoth(_.reduce(_ | _), _ => ())
      returnType <- returnTypeExpr match {
        case Some(returnTypeExpr) => resolveTypeExpr(container, returnTypeExpr)
        case None => for {
          returnType <- resolveExpr(container, bodyExpr, mutable.Map.empty, visited)
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
    conditionType <- typeCheckConstExpr(container, condition, visited)
    _ <- conditionType match {
      case BoolDatatype(_) | MutBoolDatatype(_) => Right(())
      case _ => Left(Error.semantic(s"Expected the condition to be of boolean type, found expression of type '$conditionType'", condition.range))
    }
    ifType <- typeCheckConstExpr(container, ifBlock, visited)
    elseType <- typeCheckConstExpr(container, elseBlock, visited)
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
  case (varPattern: VarPattern, _) => locals(varPattern.variable.get) = constVal
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
  case IntExpr(int, _) => Right(ConstInt(int))
  case BoolExpr(bool, _) => Right(ConstBool(bool))
  case TupleExpr(elements, _) => elements.map(e => resolveConstExpr(container, e, visited)).extract.mapBoth(_.reduce(_ | _), ConstTuple.apply)
  case blockExpr@BlockExpr(_, expr, _) =>
    blockExpr.regStmts.foreach {
      case ExprStmt(expr, _) => evaluateConstExpr(expr, locals)
      case assignment@AssignVarStmt(_, expr, _) => locals(assignment.variable.get) = evaluateConstExpr(expr, locals)
      case AssignRefStmt(_, _, _) => assert(false, "References are not allowed during compile time execution, yet")
      case LocalVarStmt(pattern, expr, _) => evaluateAssignment(pattern, evaluateConstExpr(expr, locals))
    }
    evaluateConstExpr(expr, locals)
  case UnitExpr(range) => Right(ConstUnit)
  case DotExpr(expr, iden, range) => ???
  case funExpr: FunExpr => ConstFun(funExpr)
  case IfExpr(condition, ifBlock, elseBlock, _) => evaluateConstExpr(condition, locals) match {
    case ConstBool(true) => evaluateConstExpr(ifBlock, locals)
    case ConstBool(true) => evaluateConstExpr(elseBlock, locals)
  }
}

private def typeCheckConstStmt(container: Container, constStmt: ConstStmt, visited: List[(ConstStmt, FilePosRange)]): Either[CompilerError, Unit] = {

}

private def visitConstPattern(container: Container, pattern: Pattern[Const], constVal: ConstVal): Either[CompilerError, Unit] = pattern match {
  case pattern: VarPattern[Const] => pattern.variable.get.value = Some(constVal)
  case TuplePattern(patterns, _) =>
    val ConstTuple(constVals) = constVal
    patterns.zip(constVals).map { case (p, c) => visitConstPattern(container, p, c) }.extract.mapBoth(_.reduce(_ | _), _ => ())
}

private def evaluateConstStmt(container: Container, constStmt: ConstStmt): Either[CompilerError, Unit] = if (constStmt.value.isEmpty) for {
  constVal <- evaluateConstExpr(constStmt.expr, mutable.Map.empty)
  _ <- visitConstPattern(container, constStmt.pattern, constVal)
} yield ()

def semanticAnalysis(module: Module): Either[CompilerError, Unit] = {
  resolveTypes(module)


  Right(())
}
