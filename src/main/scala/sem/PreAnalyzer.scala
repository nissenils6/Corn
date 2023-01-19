package sem

import syn.*

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
