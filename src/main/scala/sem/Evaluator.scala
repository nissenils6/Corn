package sem

import core.*
import syn.*

import scala.collection.mutable

private def evaluateAssignment(pattern: Pattern[Var], constVal: ConstVal, locals: mutable.Map[Var, ConstVal]): Unit = (pattern, constVal) match {
  case (varPattern: VarPattern[Var], _) => locals(varPattern.variable.get) = constVal
  case (TuplePattern(patterns, _), ConstTuple(values)) => patterns.zip(values).foreach { case (p, v) => evaluateAssignment(p, v, locals) }
}

private def visitAssignConstPattern(pattern: Pattern[Const], constVal: ConstVal): Either[CompilerError, Unit] = pattern match {
  case pattern: VarPattern[Const] =>
    pattern.variable.get.value = Some(constVal)
    Right(())
  case TuplePattern(patterns, _) =>
    val ConstTuple(constVals) = constVal
    patterns.zip(constVals).map { case (p, c) => visitAssignConstPattern(p, c) }.extract.mapBoth(_.reduce(_ | _), _ => ())
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
    case ConstBool(false) => evaluateConstExpr(elseBlock, locals)
  }
}
