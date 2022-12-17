package asm

enum ConditionType {
  case Gt extends ConditionType
  case Gte extends ConditionType
  case Eq extends ConditionType
  case Neq extends ConditionType
}

case class Condition(conditionType: ConditionType, left: Src, right: Src)

sealed trait Src
sealed trait Dst extends Src
sealed trait DstOff extends Dst {
    def offset(off: Int): DstOff = this match {
      case Mem(reg, offset) => Mem(reg, offset + off)
      case Loc(offset) => Loc(offset + off)
      case Glo(offset) => Glo(offset + off)
    }
}

case class Imm(imm: Long) extends Src
case class RefFun(fun: Int) extends Src
case class RefMem(reg: Int, offset: Int = 0) extends Src
case class RefLoc(offset: Int) extends Src
case class RefGlo(offset: Int) extends Src

case class Reg(reg: Int) extends Dst

case class Mem(reg: Int, offset: Int = 0) extends DstOff
case class Loc(offset: Int) extends DstOff
case class Glo(offset: Int) extends DstOff

enum SimpleOpType {
  case Add extends SimpleOpType
  case Sub extends SimpleOpType
  case And extends SimpleOpType
  case Or extends SimpleOpType
  case Xor extends SimpleOpType

  def apply(result: Dst, left: Src, right: Src): SimpleOp = SimpleOp(this, result, left, right)
}

enum DataSize {
  case Byte extends DataSize
  case Word extends DataSize
  case DWord extends DataSize
  case QWord extends DataSize
}

abstract class Op {

}

case class SimpleOp(simpleOpType: SimpleOpType, result: Dst, left: Src, right: Src) extends Op
case class MultOp(result: Dst, left: Src, right: Src) extends Op
case class DivOp(quotient: Option[Dst], remainder: Option[Dst], left: Src, right: Src) extends Op
case class MovOp(dst: Dst, src: Src) extends Op
case class IfOp(condition: Condition, ifBlock: Array[Op], elseBlock: Array[Op]) extends Op
case class CallOp(fun: Int, results: List[Dst], args: List[Src]) extends Op
case class CallIndOp(fun: Src, results: List[Dst], args: List[Src]) extends Op
case class RetOp(values: List[Src]) extends Op
case class PrintOp(arg: Src) extends Op

class Fun(val block: Array[Op], params: Int, returnValues: Int, registers: Int, stackSpace: Int)

class Program(val funs: Array[Fun], val data: Array[Byte])
