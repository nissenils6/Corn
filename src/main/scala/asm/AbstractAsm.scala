package asm

import scala.collection.mutable

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
  private def formatSimple(operator: String, operands: List[Src]): String =
    s" ${operator.padTo(12, ' ')}${operands.mkString(", ")}\n"

  private def formatSimple(operator: String, results: List[Option[Dst]], operands: List[Src]): String =
    s" ${results.map(_.map(_.toString).getOrElse("_")).mkString(", ").padTo(8, ' ')} <- $operator ${operands.mkString(", ")}\n"

  def format(): String = this match {
    case Simple(simpleOpType, result, left, right) => formatSimple( simpleOpType.name, List(Some(result)), List(left, right))
    case Mult(result, left, right) => formatSimple( "mul", List(Some(result)), List(left, right))
    case Div(quotient, remainder, left, right) => formatSimple( "div", List(quotient, remainder), List(left, right))
    case Mov(dataSize, dst, src) => formatSimple( s"mov $dataSize", List(Some(dst)), List(src))
    case If(condition, _) => s" if $condition\n"
    case Else(_, _) => s" else\n"
    case EndIf(_) => s" endif\n"
    case CSet(condition, dst) => formatSimple( s"set if $condition ", List(Some(dst)), List())
    case Call(fun, results, args) => formatSimple( s"call($fun)", results.map(Some.apply), args)
    case Ret(values) => formatSimple( "ret", values)
    case Print(arg) => formatSimple( "print", List(arg))
  }

  def sources(): List[Src] = this match {
    case Simple(_, _, left, right) => List(left, right)
    case Mult(result, left, right) => List(left, right)
    case Div(quotient, remainder, left, right) => List(left, right)
    case Mov(dataSize, dst, src) => List(src)
    case If(condition, _) => List(condition.left, condition.right)
    case Else(_, _) => List()
    case EndIf(_) => List()
    case CSet(condition, dst) => List(condition.left, condition.right)
    case Call(fun, results, args) => args
    case Ret(values) => values
    case Print(arg) => List(arg)
  }

  def registerSources(): List[Int] = sources().flatMap {
    case Reg(r) => Some(r)
    case Mem(r, _) => Some(r)
    case RefMem(r, _) => Some(r)
    case _ => None
  }
}

case class Simple(var simpleOpType: SimpleOpType, var result: Dst, var left: Src, var right: Src) extends Op
case class Mult(var result: Dst, var left: Src, var right: Src) extends Op
case class Div(var quotient: Option[Dst], var remainder: Option[Dst], var left: Src, var right: Src) extends Op
case class Mov(var dataSize: DataSize, var dst: Dst, var src: Src) extends Op
case class If(var condition: Condition, var elseInstr: Int) extends Op
case class Else(var ifInstr: Int, var endInstr: Int) extends Op
case class EndIf(var elseInstr: Int) extends Op
case class CSet(var condition: Condition, var dst: Dst) extends Op
case class Call(var fun: Src, var results: List[Dst], var args: List[Src]) extends Op
case class Ret(var values: List[Src]) extends Op
case class Print(var arg: Src) extends Op

class Fun(val block: Array[Op], val params: Int, val returnValues: Int, val registers: Int, val stackSpace: Int) {
  def format(id: Int): String = s"fun[$id] (parameters = $params, return values = $returnValues, registers = $registers, local stack space = $stackSpace)\n${block.map(_.format()).mkString}"
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

def purgeDeadCode(program: Program): Unit = program.funs.foreach { fun =>
  val reachedRegisters: mutable.Set[Int] = mutable.Set.empty

  fun.block.foreach { op =>
    reachedRegisters.addAll(op.registerSources())
  }


}
