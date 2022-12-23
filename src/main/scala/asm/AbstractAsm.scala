package asm

import core.{roundDown, roundUp}

import java.time.temporal.TemporalQueries.offset
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

  def mapReg(registerMap: Map[Int, Int]): Condition = Condition(conditionType, left.mapReg(registerMap), right.mapReg(registerMap))
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

  def mapReg(registerMap: Map[Int, Int]): Src = this match {
    case Imm(imm) => Imm(imm)
    case RefFun(fun) => RefFun(fun)
    case RefMem(reg, offset) => RefMem(registerMap(reg), offset)
    case RefLoc(offset) => RefLoc(offset)
    case RefGlo(offset) => RefGlo(offset)
  }

  def operandType(allocation: Map[Int, IntReg]): OperandType = this match {
    case Imm(imm) => OperandType.Immediate
    case RefFun(fun) => OperandType.Computed
    case RefMem(reg, offset) => OperandType.Computed
    case RefLoc(offset) => OperandType.Computed
    case RefGlo(offset) => OperandType.Computed
    case Reg(reg) => OperandType.Register
    case Mem(reg, offset) => OperandType.Memory
    case Loc(offset) => OperandType.Memory
    case Glo(offset) => OperandType.Memory
  }

  def asm(allocation: Map[Int, IntReg]): String = this match {
    case Imm(imm) => s"$imm"
    case RefFun(fun) => s"[F$fun]"
    case RefMem(reg, offset) => s"[F${allocation(reg)} + $offset]"
    case RefLoc(offset) => s"[rsp + $offset]"
    case RefGlo(offset) => s"[global_data + $offset]"
    case Reg(reg) => s"${allocation(reg)}"
    case Mem(reg, offset) => s"[F${allocation(reg)} + $offset]"
    case Loc(offset) => s"[rsp + $offset]"
    case Glo(offset) => s"[global_data + $offset]"
  }
}

sealed trait Dst extends Src {
  override def mapReg(registerMap: Map[Int, Int]): Dst = this match {
    case Reg(reg) => Reg(registerMap(reg))
  }
}

sealed trait DstOff extends Dst {
  def offset(off: Int): DstOff = this match {
    case Mem(reg, offset) => Mem(reg, offset + off)
    case Loc(offset) => Loc(offset + off)
    case Glo(offset) => Glo(offset + off)
  }

  override def mapReg(registerMap: Map[Int, Int]): DstOff = this match {
    case Mem(reg, offset) => Mem(registerMap(reg), offset)
    case Loc(offset) => Loc(offset)
    case Glo(offset) => Glo(offset)
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

enum IntReg(val qword: String, val dword: String, val word: String, val byte: String, val preserved: Boolean) {
  case RAX extends IntReg("rax", "eax", "ax", "al", false)
  case RBX extends IntReg("rbx", "ebx", "bx", "bl", true)
  case RCX extends IntReg("rcx", "ecx", "cx", "cl", false)
  case RDX extends IntReg("rdx", "edx", "dx", "dl", false)
  case RSI extends IntReg("rsi", "esi", "si", "sil", true)
  case RDI extends IntReg("rdi", "edi", "di", "dil", true)
  case RBP extends IntReg("rbp", "ebp", "bp", "bpl", true)
  case RSP extends IntReg("rsp", "esp", "sp", "spl", true)
  case R8 extends IntReg("r8", "r8d", "r8w", "r8b", false)
  case R9 extends IntReg("r9", "r9d", "r9w", "r9b", false)
  case R10 extends IntReg("r10", "r10d", "r10w", "r10b", false)
  case R11 extends IntReg("r11", "r11d", "r11w", "r11b", false)
  case R12 extends IntReg("r12", "r12d", "r12w", "r12b", true)
  case R13 extends IntReg("r13", "r13d", "r13w", "r13b", true)
  case R14 extends IntReg("r14", "r14d", "r14w", "r14b", true)
  case R15 extends IntReg("r15", "r15d", "r15w", "r15b", true)

  def apply(dataSize: DataSize): String = dataSize match {
    case DataSize.Byte => byte
    case DataSize.Word => word
    case DataSize.DWord => dword
    case DataSize.QWord => qword
  }

  override def toString: String = qword
}

trait OperandType {

}

object OperandType {
  case object Memory extends OperandType
  case object Register extends OperandType
  case object Immediate extends OperandType
  case object Computed extends OperandType
}

abstract class Op {
  private def formatSimple(operator: String): String = s" $operator\n"

  private def formatSimple(operator: String, operands: List[Src]): String =
    s" ${operator.padTo(12, ' ')}${operands.mkString(", ")}\n"

  private def formatSimple(operator: String, results: List[Option[Dst]], operands: List[Src]): String =
    s" ${results.map(_.map(_.toString).getOrElse("_")).mkString(", ").padTo(8, ' ')} <- $operator ${operands.mkString(", ")}\n"

  def format(): String = this match {
    case Simple(simpleOpType, result, left, right) => formatSimple(simpleOpType.name, List(Some(result)), List(left, right))
    case Mult(result, left, right) => formatSimple("mul", List(Some(result)), List(left, right))
    case Div(quotient, remainder, left, right) => formatSimple("div", List(quotient, remainder), List(left, right))
    case Mov(dataSize, dst, src) => formatSimple(s"mov $dataSize", List(Some(dst)), List(src))
    case If(condition, _) => s" if $condition\n"
    case Else(_, _) => formatSimple("else")
    case EndIf(_) => formatSimple("endif")
    case CSet(condition, dst) => formatSimple(s"set if $condition ", List(Some(dst)), List())
    case Call(fun, results, args) => formatSimple(s"call($fun)", results, args)
    case Ret(values) => formatSimple("ret", values)
    case Print(arg) => formatSimple("print", List(arg))
    case Nop => formatSimple("nop")
  }

  def sources(): List[Src] = this match {
    case Simple(_, _, left, right) => List(left, right)
    case Mult(_, left, right) => List(left, right)
    case Div(_, _, left, right) => List(left, right)
    case Mov(_, _, src) => List(src)
    case If(condition, _) => List(condition.left, condition.right)
    case Else(_, _) => List()
    case EndIf(_) => List()
    case CSet(condition, _) => List(condition.left, condition.right)
    case Call(fun, _, args) => fun :: args
    case Ret(values) => values
    case Print(arg) => List(arg)
  }

  def destinations(): List[Src] = this match {
    case Simple(_, result, _, _) => List(result)
    case Mult(result, _, _) => List(result)
    case Div(quotient, remainder, _, _) => List(quotient, remainder).flatten
    case Mov(_, dst, _) => List(dst)
    case If(_, _) => List()
    case Else(_, _) => List()
    case EndIf(_) => List()
    case CSet(_, dst) => List(dst)
    case Call(_, results, _) => results.flatten
    case Ret(values) => List()
    case Print(_) => List()
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
case class Call(var fun: Src, var results: List[Option[Dst]], var args: List[Src]) extends Op
case class Ret(var values: List[Src]) extends Op
case class Print(var arg: Src) extends Op
case object Nop extends Op

class Fun(var block: Array[Op], var params: Int, var returnValues: Int, var registers: Int, var stackSpace: Int) {
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

  val aliveOps = fun.block.map {
    case Simple(_, Reg(reg), _, _) => reachedRegisters.contains(reg)
    case Mult(Reg(reg), _, _) => reachedRegisters.contains(reg)
    case div: Div =>
      div.quotient = div.quotient.filter {
        case Reg(reg) => reachedRegisters.contains(reg)
        case _ => true
      }
      div.remainder = div.remainder.filter {
        case Reg(reg) => reachedRegisters.contains(reg)
        case _ => true
      }
      div.quotient.orElse(div.remainder).nonEmpty
    case Mov(_, Reg(reg), _) => reachedRegisters.contains(reg)
    case CSet(_, Reg(reg)) => reachedRegisters.contains(reg)
    case call: Call =>
      call.results = call.results.map(_.filter {
        case Reg(reg) => reachedRegisters.contains(reg)
        case _ => true
      })
      true
    case _ => true
  }

  fun.block = fun.block.zip(aliveOps).map {
    case (Simple(simpleOpType, result, Imm(left), right), true) if simpleOpType != Sub => Simple(simpleOpType, result, right, Imm(left))
    case (Mult(result, Imm(left), right), true) => Mult(result, right, Imm(left))
    case (op, true) => op
    case (_, false) => Nop
  }

  val registerMap = Range(0, fun.registers).filter(r => reachedRegisters.contains(r) || r <= fun.params).zipWithIndex.toMap

  fun.block.foreach {
    case simpleOp: Simple =>
      simpleOp.result = simpleOp.result.mapReg(registerMap)
      simpleOp.left = simpleOp.left.mapReg(registerMap)
      simpleOp.right = simpleOp.right.mapReg(registerMap)
    case multOp: Mult =>
      multOp.result = multOp.result.mapReg(registerMap)
      multOp.left = multOp.left.mapReg(registerMap)
      multOp.right = multOp.right.mapReg(registerMap)
    case divOp: Div =>
      divOp.quotient = divOp.quotient.map(_.mapReg(registerMap))
      divOp.remainder = divOp.remainder.map(_.mapReg(registerMap))
      divOp.left = divOp.left.mapReg(registerMap)
      divOp.right = divOp.right.mapReg(registerMap)
    case movOp: Mov =>
      movOp.dst = movOp.dst.mapReg(registerMap)
      movOp.src = movOp.src.mapReg(registerMap)
    case ifOp: If =>
      ifOp.condition = ifOp.condition.mapReg(registerMap)
    case Else(_, _) => ()
    case EndIf(_) => ()
    case cSetOp: CSet =>
      cSetOp.condition = cSetOp.condition.mapReg(registerMap)
      cSetOp.dst = cSetOp.dst.mapReg(registerMap)
    case callOp: Call =>
      callOp.fun = callOp.fun.mapReg(registerMap)
      callOp.results = callOp.results.map(_.map(_.mapReg(registerMap)))
      callOp.args = callOp.args.map(_.mapReg(registerMap))
    case retOp: Ret =>
      retOp.values = retOp.values.map(_.mapReg(registerMap))
    case printOp: Print =>
      printOp.arg = printOp.arg.mapReg(registerMap)
    case Nop => ()
  }

  fun.registers = registerMap.size
}

val X64_REGS_FOR_ALLOC: Array[IntReg] = Array(IntReg.RCX, IntReg.R8, IntReg.R9, IntReg.R10, IntReg.R11, IntReg.RBX, IntReg.RDI, IntReg.RSI, IntReg.RBP, IntReg.R12, IntReg.R13, IntReg.R14, IntReg.R15)

def assembleX64WindowsWithLinearScan(program: Program): String = {
  program.funs.foreach { fun =>
    val divInstructions = fun.block.zipWithIndex.filter(_._1.isInstanceOf[Div]).map(_._2).toSet

    val registerWeights = Array.fill(fun.registers)(0)

    val startRanges = Array.fill(fun.registers)(fun.block.length)
    val endRanges = Array.fill(fun.registers)(0)

    var ifDepth = 0

    def useReg(reg: Int, op: Int): Unit = {
      startRanges(reg) = startRanges(reg).min(op)
      endRanges(reg) = endRanges(reg).max(op)

      val weight = 256 >> ifDepth.min(8)
      registerWeights(reg) += weight
    }

    fun.block.zipWithIndex.foreach { case (op, idx) =>
      op.sources().foreach {
        case RefMem(reg, _) => useReg(reg, idx)
        case Reg(reg) => useReg(reg, idx)
        case Mem(reg, _) => useReg(reg, idx)
        case _ => ()
      }

      op.destinations().foreach {
        case Reg(reg) => useReg(reg, idx)
        case Mem(reg, _) => useReg(reg, idx)
        case _ => ()
      }

      op match {
        case If(_, _) => ifDepth += 1
        case EndIf(_) => ifDepth -= 1
        case _ => ()
      }
    }

    val ranges = (0 until fun.registers).zip(startRanges.zip(endRanges)).sortBy(_._2._1).toArray

    val regToRange = ranges.toMap
    val freeRegs = mutable.Set.from(X64_REGS_FOR_ALLOC)
    var spilledCount = 0
    val allocedRegs: mutable.Map[Int, Either[Int, IntReg]] = mutable.Map.empty
    val touchedRegs: mutable.Set[Int] = mutable.Set.empty

    var active: List[Int] = List.empty

    def spill(): Unit = {
      val (reg: Int, Right(asmReg: IntReg)) = allocedRegs.maxBy {
        case (reg, Right(_)) =>
          val (s, e) = regToRange(reg)
          registerWeights(reg) / (2 + e - s)
        case _ => -1
      }
      freeRegs.add(asmReg)
      allocedRegs(reg) = Left(spilledCount)
      spilledCount += 1
    }

    def alloc(reg: Int, useRdx: Boolean): Unit = if (freeRegs.isEmpty) {
      spill()
      alloc(reg, useRdx)
    } else {
      X64_REGS_FOR_ALLOC.find { newReg =>
        freeRegs.contains(newReg) && (newReg != IntReg.RDX || useRdx)
      } match {
        case Some(allocedReg) =>
          allocedRegs(reg) = Right(allocedReg)
          touchedRegs.add(reg)
          freeRegs.remove(allocedReg)
          active = reg :: active
        case None =>
          spill()
          alloc(reg, useRdx)
      }
    }

    ranges.foreach { case (reg, (start, end)) =>
      val (toDealloc, newActive) = active.partition { reg =>
        val activeEnd = endRanges(reg)
        activeEnd <= start
      }
      active = newActive
      toDealloc.foreach { reg =>
        allocedRegs(reg).foreach(freeRegs.add)
      }

      val useRdx = !divInstructions.exists((start until end).contains)
      alloc(reg, useRdx)
    }

    def spillAsm(spillSlot: Int) = s"qword[rsp + ${fun.stackSpace.roundUp(8) + spillSlot * 8}]"

    def asm(operator: String, operands: String*) = s"        ${operator.padTo(8, ' ')}${operands.mkString(", ")}\n"

    //    def operation(operator: String, dstOperand: String, srcOperand: String, dstType: OperandType, srcType: OperandType) = (dstType, srcType) match {
    //      case (OperandType.Register, OperandType.Immediate) => asm(operator, dstOperand, srcOperand)
    //      case (OperandType.Register, OperandType.Computed) => asm("lea", "rax", srcOperand) + asm(operator, dstOperand, "rax")
    //      case (OperandType.Register, OperandType.Register) => asm(operator, dstOperand, srcOperand)
    //      case (OperandType.Register, OperandType.Memory) => asm(operator, dstOperand, srcOperand)
    //      case (OperandType.Memory, OperandType.Immediate) => asm(operator, dstOperand, srcOperand)
    //      case (OperandType.Memory, OperandType.Computed) => asm("lea", "rax", srcOperand) + asm(operator, dstOperand, "rax")
    //      case (OperandType.Memory, OperandType.Register) => asm(operator, dstOperand, srcOperand)
    //      case (OperandType.Memory, OperandType.Memory) => asm("mov", )
    //    }

    val asmText = fun.block.map {
      case Simple(simpleOpType, Reg(dst), Reg(left), right) if dst == left => (allocedRegs(dst), right) match {
        case (Left(spillSlot), Imm(imm)) => asm(simpleOpType.name, spillAsm(spillSlot), s"$imm")
        case (Right(asmReg), Imm(imm)) => asm(simpleOpType.name, asmReg.qword, s"$imm")
        case (Left(spillSlot), src) => assert(false, s"HANDLING OF $src FOR SIMPLE OPERATIONS WITH SPILLED DESTINATION REGISTER IS NOT IMPLEMENTED YET")
        case (Right(asmReg), src) => assert(false, s"HANDLING OF $src FOR SIMPLE OPERATIONS WITH NON-SPILLED DESTINATION REGISTER IS NOT IMPLEMENTED YET")
      }
      case Mov(dataSize, Reg(reg), right) => (allocedRegs(reg), right) match {
        case (Left(spillSlot), Imm(imm)) => asm(s"mov $dataSize", spillAsm(spillSlot), s"$imm")
        case (Right(asmReg), Imm(imm)) => asm(s"mov $dataSize", asmReg.qword, s"$imm")
        case (Left(spillSlot), Loc(offset)) =>
          asm(s"mov $dataSize", IntReg.RAX(dataSize), s"[rsp + $offset]") +
            asm(s"mov $dataSize", spillAsm(spillSlot), IntReg.RAX(dataSize))
        case (Right(asmReg), Loc(offset)) => asm(s"mov $dataSize", asmReg.qword, s"[rsp + $offset]")
        case (Left(spillSlot), src) => assert(false, s"HANDLING OF $src FOR MOV WITH SPILLED DESTINATION REGISTER IS NOT IMPLEMENTED YET")
        case (Right(asmReg), src) => assert(false, s"HANDLING OF $src FOR MOV WITH NON-SPILLED DESTINATION REGISTER IS NOT IMPLEMENTED YET")
      }
      case Mov(dataSize, Loc(offset), Imm(imm)) => asm(s"mov $dataSize", s"[rsp + $offset]", s"$imm")
      case Mov(dataSize, Loc(dstOffset), Loc(srcOffset)) =>
        asm(s"mov $dataSize", IntReg.RAX(dataSize), s"[rsp + $srcOffset]") +
          asm(s"mov $dataSize", s"[rsp + $dstOffset]", IntReg.RAX(dataSize))
      case op => assert(false, s"HANDLING OF $op IS NOT IMPLEMENTED YET")
    }.mkString

    println(asmText)
  }

  ""
}
