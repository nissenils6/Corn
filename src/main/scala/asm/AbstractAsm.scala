package asm

enum CondType(val name: String) {
  case Gt extends CondType(">")
  case Gte extends CondType(">=")
  case Eq extends CondType("==")
  case Neq extends CondType("!=")

  override def toString: String = name
}

case class Condition(conditionType: CondType, left: Src, right: Src) {
  override def toString: String = s"$left $conditionType $right"
}

sealed trait Src {
  override def toString: String = this match {
    case Imm(imm) => s"$imm"
    case RefFun(fun) => s"&fun[$fun]"
    case RefMem(reg, offset) => s"&[r$reg + 0x${offset.toHexString}]"
    case RefLoc(offset) => s"&loc[0x${offset.toHexString}]"
    case RefGlo(offset) => s"&glo[0x${offset.toHexString}]"
    case Reg(reg) => s"r$reg"
    case Mem(reg, offset) => s"&[r$reg + 0x${offset.toHexString}]"
    case Loc(offset) => s"loc[0x${offset.toHexString}]"
    case Glo(offset) => s"glo[0x${offset.toHexString}]"
  }
}

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

abstract class SimpleOpType(val name: String) {
  def apply(result: Dst, left: Src, right: Src): Simple = Simple(this, result, left, right)
  override def toString: String = name
}

case object Add extends SimpleOpType("add")
case object Sub extends SimpleOpType("sub")
case object And extends SimpleOpType("and")
case object Or extends SimpleOpType("or")
case object Xor extends SimpleOpType("xor")

enum DataSize(val size: Int, val name: String) {
  case Byte extends DataSize(1, "byte")
  case Word extends DataSize(2, "word")
  case DWord extends DataSize(4, "dword")
  case QWord extends DataSize(8, "qword")

  override def toString: String = name
}

abstract class Op {
  private def formatSimple(indentation: Int, operator: String, operands: List[Src]): String =
    s"${" " * indentation}${operator.padTo(12, ' ')}${operands.mkString(", ")}\n"

  private def formatSimple(indentation: Int, operator: String, results: List[Option[Dst]], operands: List[Src]): String =
    s"${" " * indentation}${results.map(_.map(_.toString).getOrElse("_")).mkString(", ").padTo(8, ' ')} <- $operator ${operands.mkString(", ")}\n"

  def format(indentation: Int): String = this match {
    case Simple(simpleOpType, result, left, right) => formatSimple(indentation, simpleOpType.name, List(Some(result)), List(left, right))
    case Mult(result, left, right) => formatSimple(indentation, "mul", List(Some(result)), List(left, right))
    case Div(quotient, remainder, left, right) => formatSimple(indentation, "div", List(quotient, remainder), List(left, right))
    case Mov(dataSize, dst, src) => formatSimple(indentation, s"mov $dataSize", List(Some(dst)), List(src))
    case If(condition, ifBlock, elseBlock) => s"${" " * indentation}if $condition then\n${ifBlock.map(_.format(indentation + 1)).mkString}${" " * indentation}else\n${elseBlock.map(_.format(indentation + 1)).mkString}${" " * indentation}end\n"
    case Call(fun, results, args) => formatSimple(indentation, s"call($fun)", results.map(Some.apply), args)
    case Ret(values) => formatSimple(indentation, "ret", values)
    case Print(arg) => formatSimple(indentation, "print", List(arg))
  }
}

case class Simple(simpleOpType: SimpleOpType, result: Dst, left: Src, right: Src) extends Op
case class Mult(result: Dst, left: Src, right: Src) extends Op
case class Div(quotient: Option[Dst], remainder: Option[Dst], left: Src, right: Src) extends Op
case class Mov(dataSize: DataSize, dst: Dst, src: Src) extends Op
case class If(condition: Condition, ifBlock: Array[Op], elseBlock: Array[Op]) extends Op
case class Call(fun: Src, results: List[Dst], args: List[Src]) extends Op
case class Ret(values: List[Src]) extends Op
case class Print(arg: Src) extends Op

class Fun(val block: Array[Op], val params: Int, val returnValues: Int, val registers: Int, val stackSpace: Int) {
  def format(id: Int): String = s"fun[$id] (parameters = $params, return values = $returnValues, registers = $registers, local stack space = $stackSpace)\n${block.map(_.format(1)).mkString}"
}

class Program(val funs: Array[Fun], val data: Array[Byte]) {
  override def toString: String = s"${
    funs.zipWithIndex.map { case (fun, id) =>
      fun.format(id)
    }.mkString
  }\nglobal data\n${
    data.grouped(64).zipWithIndex.map { case (group, idx) =>
      s" 0x${(idx * 64).toHexString.reverse.padTo(4, '0').reverse}: ${group.map(b => "0x" + b.toInt.toHexString.reverse.padTo(2, '0').reverse).mkString(" ")}"
    }.mkString("\n")
  }"
}
