package sem

import syn.*

private def evalBinary[P, R](p: ConstVal => P, r: R => ConstVal)(f: (P, P) => R)(list: List[ConstVal]): ConstVal = {
  val List(a, b) = list.map(p)
  r(f(a, b))
}

private val evalBinaryInt = evalBinary(_.toInt, ConstInt.apply)
private val evalCompInt = evalBinary(_.toInt, ConstBool.apply)

private val unaryInt =List(IntDatatype)
private val binaryInt = List(IntDatatype, IntDatatype)

private def injectType(module: Module, name: String, datatype: Datatype): Unit = {
  val typeVar = TypeVar(name, None)
  typeVar.value = Some(datatype)
  module.addType(typeVar)
}

def injectBinaryOp(module: Module, name: String, eval: List[ConstVal] => ConstVal, params: List[Datatype], returnType: Datatype, graphOp: (opt.Data, opt.Data) => opt.OpNext, index: Int = 0): Unit = {
  val op = graphOp(opt.Data(None, 0), opt.Data(None, 1))
  op.next = opt.Ret(List(opt.Data(Some(op), index)))
  val optFun = Some(new opt.Fun(op, opt.FunDatatype(params.map(_.optDatatype), List(returnType.optDatatype)), List()))

  val signature = FunDatatype(params, returnType, false)
  val globalConst = Const(name, signature)
  globalConst.value = Some(ConstFun(BuiltinFun(params, returnType, Some(eval), optFun)))
  module.addConst(globalConst)
}

def injectCompOp(module: Module, name: String, eval: List[ConstVal] => ConstVal, params: List[Datatype], returnType: Datatype, graphOp: opt.Data => opt.OpNext, swapped: Boolean): Unit = {
  val subOp = opt.AddInt(List(opt.Data(None, if swapped then 1 else 0)), List(opt.Data(None, if swapped then 0 else 1)))
  val op = graphOp(opt.Data(subOp))
  subOp.next = op
  op.next = opt.Ret(List(opt.Data(op)))
  val optFun = Some(new opt.Fun(subOp, opt.FunDatatype(params.map(_.optDatatype), List(returnType.optDatatype)), List()))

  val signature = FunDatatype(params, returnType, false)
  val globalConst = Const(name, signature)
  globalConst.value = Some(ConstFun(BuiltinFun(params, returnType, Some(eval), optFun)))
  module.addConst(globalConst)
}

def injectPrintln(module: Module): Unit = {
  val op = opt.Print(opt.Data(None, 0))
  op.next = opt.Ret(List(opt.Data(op)))
  val optFun = Some(new opt.Fun(op, opt.FunDatatype(List(opt.IntDatatype), List(opt.UnitDatatype)), List()))

  val signature = FunDatatype(unaryInt, IntDatatype, false)
  val globalConst = Const("println", signature)
  globalConst.value = Some(ConstFun(BuiltinFun(unaryInt, IntDatatype, None, optFun)))
  module.addConst(globalConst)
}

def injectBuiltins(module: Module): Unit = {
  injectType(module, "Int", IntDatatype)
  injectType(module, "Bool", BoolDatatype)

  injectBinaryOp(module, "+", evalBinaryInt(_ + _), binaryInt, IntDatatype, (d0, d1) => opt.AddInt(List(d0, d1), List()))
  injectBinaryOp(module, "-", evalBinaryInt(_ - _), binaryInt, IntDatatype, (d0, d1) => opt.AddInt(List(d0), List(d1)))
  injectBinaryOp(module, "&", evalBinaryInt(_ & _), binaryInt, IntDatatype, (d0, d1) => opt.BitwiseInt(opt.BitwiseOp.And, List(d0, d1)))
  injectBinaryOp(module, "|", evalBinaryInt(_ | _), binaryInt, IntDatatype, (d0, d1) => opt.BitwiseInt(opt.BitwiseOp.Or, List(d0, d1)))
  injectBinaryOp(module, "^", evalBinaryInt(_ ^ _), binaryInt, IntDatatype, (d0, d1) => opt.BitwiseInt(opt.BitwiseOp.Xor, List(d0, d1)))
  injectBinaryOp(module, "*", evalBinaryInt(_ * _), binaryInt, IntDatatype, (d0, d1) => opt.MultInt(List(d0, d1)))
  injectBinaryOp(module, "/", evalBinaryInt(_ / _), binaryInt, IntDatatype, opt.DivInt.apply)
  injectBinaryOp(module, "%", evalBinaryInt(_ % _), binaryInt, IntDatatype, opt.DivInt.apply, 1)

  injectCompOp(module, ">", evalCompInt(_ > _), binaryInt, BoolDatatype, data => opt.CompInt(opt.CompType.Pos, data), false)
  injectCompOp(module, "<", evalCompInt(_ < _), binaryInt, BoolDatatype, data => opt.CompInt(opt.CompType.Pos, data), true)
  injectCompOp(module, ">=", evalCompInt(_ >= _), binaryInt, BoolDatatype, data => opt.CompInt(opt.CompType.PosOrZero, data), false)
  injectCompOp(module, "<=", evalCompInt(_ <= _), binaryInt, BoolDatatype, data => opt.CompInt(opt.CompType.PosOrZero, data), true)
  injectCompOp(module, "==", evalCompInt(_ == _), binaryInt, BoolDatatype, data => opt.CompInt(opt.CompType.Zero, data), false)
  injectCompOp(module, "!=", evalCompInt(_ != _), binaryInt, BoolDatatype, data => opt.CompInt(opt.CompType.NotZero, data), false)

  injectPrintln(module)
}
