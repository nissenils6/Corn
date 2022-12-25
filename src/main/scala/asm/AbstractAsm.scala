package asm

import core.{roundDown, roundUp}

import java.time.temporal.TemporalQueries.offset
import scala.collection.mutable
import scala.compiletime.ops.string

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

  def compute(a: Long, b: Long): Long = this match {
    case Add => a + b
    case Sub => a - b
    case And => a & b
    case Or => a | b
    case Xor => a ^ b
  }

  override def toString: String = name
}

case object Add extends SimpleOpType("add")
case object Sub extends SimpleOpType("sub")
case object And extends SimpleOpType("and")
case object Or extends SimpleOpType("or")
case object Xor extends SimpleOpType("xor")

enum DataSize(val size: Int, val name: String, val zx: String, val sx: String) {
  case Byte extends DataSize(1, "byte", "movzx", "movsx")
  case Word extends DataSize(2, "word", "movzx", "movsx")
  case DWord extends DataSize(4, "dword", "mov", "movsx")
  case QWord extends DataSize(8, "qword", "mov", "mov")

  override def toString: String = name
}

enum AsmReg(val qword: String, val dword: String, val word: String, val byte: String, val preserved: Boolean) {
  case RAX extends AsmReg("rax", "eax", "ax", "al", false)
  case RBX extends AsmReg("rbx", "ebx", "bx", "bl", true)
  case RCX extends AsmReg("rcx", "ecx", "cx", "cl", false)
  case RDX extends AsmReg("rdx", "edx", "dx", "dl", false)
  case RSI extends AsmReg("rsi", "esi", "si", "sil", true)
  case RDI extends AsmReg("rdi", "edi", "di", "dil", true)
  case RBP extends AsmReg("rbp", "ebp", "bp", "bpl", true)
  case RSP extends AsmReg("rsp", "esp", "sp", "spl", true)
  case R8 extends AsmReg("r8", "r8d", "r8w", "r8b", false)
  case R9 extends AsmReg("r9", "r9d", "r9w", "r9b", false)
  case R10 extends AsmReg("r10", "r10d", "r10w", "r10b", false)
  case R11 extends AsmReg("r11", "r11d", "r11w", "r11b", false)
  case R12 extends AsmReg("r12", "r12d", "r12w", "r12b", true)
  case R13 extends AsmReg("r13", "r13d", "r13w", "r13b", true)
  case R14 extends AsmReg("r14", "r14d", "r14w", "r14b", true)
  case R15 extends AsmReg("r15", "r15d", "r15w", "r15b", true)

  def apply(dataSize: DataSize): String = dataSize match {
    case DataSize.Byte => byte
    case DataSize.Word => word
    case DataSize.DWord => dword
    case DataSize.QWord => qword
  }

  override def toString: String = qword
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

val X64_REGS_FOR_ALLOC: Array[AsmReg] = Array(AsmReg.RCX, AsmReg.R8, AsmReg.R9, AsmReg.R10, AsmReg.R11, AsmReg.RBX, AsmReg.RDI, AsmReg.RSI, AsmReg.RBP, AsmReg.R12, AsmReg.R13, AsmReg.R14, AsmReg.R15)

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
    val allocedRegs: mutable.Map[Int, Either[Int, AsmReg]] = mutable.Map.empty
    val touchedRegs: mutable.Set[Int] = mutable.Set.empty

    var active: List[Int] = List.empty

    def spill(): Unit = {
      val (reg: Int, Right(asmReg: AsmReg)) = allocedRegs.maxBy {
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
        freeRegs.contains(newReg) && (newReg != AsmReg.RDX || useRdx)
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

    def spillAsm(spillSlot: Int) = s"[rsp + ${fun.stackSpace.roundUp(8) + spillSlot * 8}]"

    def asm(operator: String, operands: String*) = s"        ${operator.padTo(8, ' ')}${operands.mkString(", ")}\n"

    object OperandImm {
      def unapply(src: Src): Option[String] = src match {
        case Imm(imm) => Some(s"$imm")
        case _ => None
      }
    }

    object OperandReg {
      def unapply(src: Src): Option[String] = src match {
        case Reg(reg) => allocedRegs(reg) match {
          case Left(_) => None
          case Right(asmReg) => Some(s"${asmReg.qword}")
        }
        case _ => None
      }
    }

    object OperandMem {
      def unapply(src: Src): Option[String] = src match {
        case Reg(reg) => allocedRegs(reg) match {
          case Left(spillSlot) => Some(spillAsm(spillSlot))
          case Right(_) => None
        }
        case Mem(reg, offset) => allocedRegs(reg) match {
          case Left(_) => None
          case Right(asmReg) => Some(s"[${asmReg.qword} + $offset]")
        }
        case Loc(offset) => Some(s"[rsp + $offset]")
        case Glo(offset) => Some(s"[global_data + $offset]")
        case _ => None
      }
    }

    object OperandImmOrReg {
      def unapply(src: Src): Option[String] = src match {
        case OperandImm(string) => Some(string)
        case OperandReg(string) => Some(string)
        case _ => None
      }
    }

    object OperandMemOrReg {
      def unapply(src: Src): Option[String] = src match {
        case OperandMem(string) => Some(string)
        case OperandReg(string) => Some(string)
        case _ => None
      }
    }

    object OperandAny {
      def unapply(src: Src): Option[String] = src match {
        case OperandMem(string) => Some(string)
        case OperandReg(string) => Some(string)
        case OperandImm(string) => Some(string)
        case _ => None
      }
    }

    object OperandMemComputed {
      def unapply(src: Src): Option[String => (String, String)] = src match {
        case Mem(reg, offset) => allocedRegs(reg) match {
          case Left(spillSlot) => Some(tempReg => (
            asm("mov", tempReg, spillAsm(spillSlot)),
            s"[$tempReg + $offset]"
          ))
          case Right(_) => None
        }
        case _ => None
      }
    }

    object OperandRegComputed {
      def unapply(src: Src): Option[String => String] = src match {
        case RefFun(fun) => Some(tempReg => asm("lea", tempReg, s"[F$fun]"))
        case RefMem(reg, offset) => allocedRegs(reg) match {
          case Left(spillSlot) => Some(tempReg => asm("mov", tempReg, spillAsm(spillSlot)) + asm("lea", tempReg, s"[$tempReg + $offset]"))
          case Right(asmReg) => Some(tempReg => asm("lea", tempReg, s"[${asmReg.qword} + $offset]"))
        }
        case RefLoc(offset) => Some(tempReg => asm("lea", tempReg, s"[rsp + $offset]"))
        case RefGlo(offset) => Some(tempReg => asm("lea", tempReg, s"[global_data + $offset]"))
        case _ => None
      }
    }

    object OperandAnyComputed {
      def unapply(src: Src): Option[String => (String, String)] = src match {
        case OperandMemComputed(stringFunction) => Some(stringFunction)
        case OperandRegComputed(stringFunction) => Some(string => (stringFunction(string), string))
        case _ => None
      }
    }

    // TODO: Add support for computed destinations
    def binaryOp2(name: String, dst: Dst, src: Src) = (dst, src) match {
      case (OperandReg(left), OperandAny(right)) => asm(name, left, right)
      case (OperandReg(left), OperandAnyComputed(computed)) =>
        val (stmt, right) = computed(AsmReg.RAX.qword)
        stmt + asm(name, left, right)
      case (OperandMem(left), OperandImmOrReg(right)) => asm(name, left, right)
      case (OperandMem(left), OperandMem(right)) => asm("mov", AsmReg.RAX.qword, right) + asm(name, left, AsmReg.RAX.qword)
      case (OperandMem(left), OperandRegComputed(computed)) => computed(AsmReg.RAX.qword) + asm(name, left, AsmReg.RAX.qword)
      case (OperandMem(left), OperandMemComputed(computed)) =>
        val (stmt, right) = computed(AsmReg.RAX.qword)
        stmt + asm("mov", AsmReg.RAX.qword, right) + asm(name, left, AsmReg.RAX.qword)
      case _ => assert(false, s"HANDLING OF $dst AND $src IS NOT IMPLEMENTED YET")
    }

    // TODO: Add support for everything
    def binaryOp3(name: String, dst: Dst, left: Src, right: Src) = (dst, left, right) match {
      case (OperandReg(dst), OperandReg(left), OperandImmOrReg(right)) => asm("mov", dst, left) + asm(name, dst, right)
      case _ => assert(false, s"HANDLING OF $dst, $left AND $right IS NOT IMPLEMENTED YET")
    }

    def movOp(dataSize: DataSize, dst: Dst, src: Src) = (dataSize, dst, src) match {
      case (_, OperandReg(dst), OperandReg(src)) if dst == src => ""
      case (_, dst, src) if dst == src => ""
      case (_, OperandReg(dst), OperandImmOrReg(src)) => asm("mov", dst, src)
      case (dataSize, OperandReg(dst), OperandMem(src)) => asm(dataSize.zx, dst, dataSize.name + src)
      case (dataSize, OperandReg(dst), OperandRegComputed(computed)) => computed(dst)
      case (dataSize, OperandReg(dst), OperandMemComputed(computed)) =>
        val (stmt, src) = computed(AsmReg.RAX.qword)
        stmt + asm(dataSize.zx, dst, dataSize.name + src)
      case (dataSize, OperandMem(dst), OperandImmOrReg(src)) => asm("mov", dataSize.name + dst, src)
      case (dataSize, OperandMem(dst), OperandMem(src)) => asm(dataSize.zx, AsmReg.RAX.qword, dataSize.name + src) + asm("mov", dataSize.name + dst, AsmReg.RAX(dataSize))
      case (dataSize, OperandMem(dst), OperandRegComputed(computed)) => computed(AsmReg.RAX.qword) + asm("mov", dataSize.name + dst, AsmReg.RAX(dataSize))
      case (dataSize, OperandMem(dst), OperandMemComputed(computed)) =>
        val (stmt, src) = computed(AsmReg.RAX.qword)
        stmt + asm(dataSize.zx, AsmReg.RAX.qword, dataSize.name + src) + asm("mov", dataSize.name + dst, AsmReg.RAX(dataSize))
      case _ => assert(false, s"HANDLING OF $dataSize, $dst AND $src IS NOT IMPLEMENTED YET")
    }

    // TODO: Make simple and mult op dispatch into binaryOp2 even for different registers if the registers are allocated to the same hardware register
    val asmText = fun.block.map {
      case Simple(simpleOpType, result, left, right) if result == left => binaryOp2(simpleOpType.name, result, right)
      case Simple(simpleOpType, result, left, right) if result == right => binaryOp2(simpleOpType.name, result, left)
      case Simple(simpleOpType, result, Imm(left), Imm(right)) => movOp(DataSize.QWord, result, Imm(simpleOpType.compute(left, right)))
      case Simple(simpleOpType, result, left, right) => binaryOp3(simpleOpType.name, result, left, right)
      case Mult(result, left, right) if result == left => binaryOp2("imul", result, right)
      case Mult(result, left, right) if result == right => binaryOp2("imul", result, left)
      case Mult(result, Imm(left), Imm(right)) => movOp(DataSize.QWord, result, Imm(left * right))
      case Mult(result, left, right) => binaryOp3("imul", result, left, right)
      case Div(quotient, remainder, left, right) => ???
      case Mov(dataSize, dst, src) => movOp(dataSize, dst, src)
      case Print(OperandAny(src)) => asm("print", src)
      case Print(OperandAnyComputed(computed)) =>
        val (stmt, src) = computed(AsmReg.RAX.qword)
        stmt + asm("print", src)
      case Ret(values) => asm("ret")
      case op => assert(false, s"HANDLING OF $op IS NOT IMPLEMENTED YET")
    }.mkString

    println(asmText)
  }

  ""
}
