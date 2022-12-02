package opt

import scala.compiletime.ops.int



def globalVarInline(optUnit: OptUnit): Unit = {
  val constVars: Map[Var, ConstVal] = optUnit.vars.map { globalVar => (globalVar, globalVar.entry.op) }.collect {
    case (globalVar, intLit@IntLit(int, Controlflow(Ret(List(returnValue))))) if returnValue.value.contains(intLit) => (globalVar, ConstInt(int))
    case (globalVar, boolLit@BoolLit(bool, Controlflow(Ret(List(returnValue))))) if returnValue.value.contains(boolLit) => (globalVar, ConstBool(bool))
    case (globalVar, funLit@FunLit(fun, Controlflow(Ret(List(returnValue))))) if returnValue.value.contains(funLit) => (globalVar, ConstFun(fun))
  }.toMap

  optUnit.transformFuns {
    case (ReadGlobal(globalVar, idx, next), mapCtrl, mapData) if constVars.contains(globalVar) =>
      lazy val (a, b) = constVars(globalVar).toGraph(next)
  }
}
