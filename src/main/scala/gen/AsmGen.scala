package gen

import sem.*

import scala.annotation.targetName
import scala.collection.mutable

abstract class Instr {
  def comment: Option[String]

  def redundant: Boolean = this match {
    case SimpleOpImm(Add | Sub | Or | Xor, _, 0, _) => true
    case SimpleOpMemImm(Add | Sub | Or | Xor, _, 0, _) => true
    case Mov(dst, src, _) => dst == src
    case _ => false
  }
  override def toString: String = withComment(this match {
    case Label(label, _) => s"$label:"
    case _ =>
      val (instr, operands) = format
      " " * 8 + instr.padTo(8, ' ') + operands
  })
  def format: (String, String) = this match {
    case SimpleOpReg(op, dst, src, _) => (op.name, s"$dst, $src")
    case SimpleOpImm(op, dst, imm, _) => (op.name, s"$dst, $imm")
    case SimpleOpAddress(op, dst, src, _) => (op.name, s"$dst, $src")
    case SimpleOpMem(op, dst, src, _) => (op.name, s"$dst, $src")
    case SimpleOpMemImm(op, dst, imm, _) => (op.name, s"$dst, $imm")
    case Neg(reg, _) => ("neg", s"$reg")
    case Imul(dst, src, _) => ("imul", s"$dst, $src")
    case Idiv(dst, _) => ("idiv", s"$dst")
    case Mov(dst, src, _) => ("mov", s"$dst, $src")
    case Load(dst, address, regSize, _) => ("mov", s"${dst(regSize)}, $address")
    case LoadImm(dst, imm, regSize, _) => ("mov", s"${dst(regSize)}, $imm")
    case Store(address, src, regSize, _) => ("mov", s"$regSize$address, ${src(regSize)}")
    case StoreImm(address, imm, regSize, _) => ("mov", s"$regSize$address, $imm")
    case Lea(dst, address, _) => ("lea", s"$dst, $address")
    case DirJump(target, _) => ("jmp", s"$target")
    case DirCondJump(target, flag, _) => (s"j${flag.name}", s"$target")
    case IndJump(address, _) => ("jmp", s"$address")
    case DirCall(target, _) => ("call", s"$target")
    case IndRegCall(reg, _) => ("call", s"$reg")
    case IndCall(address, _) => ("call", s"qword$address")
    case SetCond(reg, flag, _) => (s"set${flag.name}", s"${reg.byte}")
    case Push(reg, _) => ("push", s"$reg")
    case Pop(reg, _) => ("pop", s"$reg")
    case Ret(_) => ("ret", "")
    case Label(_, _) => ("", "")
  }
  def withComment(string: String): String = comment match {
    case Some(comment) => string.padTo(' ', 64).mkString + comment + "\n"
    case None => string + "\n"
  }
}

case class SimpleOpReg(op: SimpleOp, dst: Reg, src: Reg, comment: Option[String] = None) extends Instr

case class SimpleOpImm(op: SimpleOp, dst: Reg, src: Int, comment: Option[String] = None) extends Instr

case class SimpleOpAddress(op: SimpleOp, dst: Reg, src: Address, comment: Option[String] = None) extends Instr

case class SimpleOpMem(op: SimpleOp, dst: Address, src: Reg, comment: Option[String] = None) extends Instr

case class SimpleOpMemImm(op: SimpleOp, dst: Address, src: Int, comment: Option[String] = None) extends Instr

case class Neg(reg: Reg, comment: Option[String] = None) extends Instr

case class Imul(dst: Reg, src: Reg, comment: Option[String] = None) extends Instr

case class Idiv(src: Reg, comment: Option[String] = None) extends Instr

case class Mov(dst: Reg, src: Reg, comment: Option[String] = None) extends Instr

case class Load(dst: Reg, address: Address, regSize: RegSize = RegSize.QWord, comment: Option[String] = None) extends Instr

case class LoadImm(dst: Reg, imm: Long, regSize: RegSize = RegSize.QWord, comment: Option[String] = None) extends Instr

case class Store(address: Address, src: Reg, regSize: RegSize = RegSize.QWord, comment: Option[String] = None) extends Instr

case class StoreImm(address: Address, imm: Int, regSize: RegSize = RegSize.QWord, comment: Option[String] = None) extends Instr

case class Lea(dst: Reg, address: Address, comment: Option[String] = None) extends Instr

case class DirJump(target: String, comment: Option[String] = None) extends Instr

case class DirCondJump(target: String, flag: Flag, comment: Option[String] = None) extends Instr

case class IndJump(address: Address, comment: Option[String] = None) extends Instr

case class SetCond(dst: Reg, flag: Flag, comment: Option[String] = None) extends Instr

case class DirCall(target: String, comment: Option[String] = None) extends Instr

case class IndRegCall(reg: Reg, comment: Option[String] = None) extends Instr

case class IndCall(address: Address, comment: Option[String] = None) extends Instr

case class Ret(comment: Option[String] = None) extends Instr

case class Push(reg: Reg, comment: Option[String] = None) extends Instr

case class Pop(reg: Reg, comment: Option[String] = None) extends Instr

case class Label(label: String, comment: Option[String] = None) extends Instr

abstract class Address {
  override def toString: String = this match {
    case Dis(dis, label) => s"[${formatDis(dis, label)}]"
    case Base(base) => s"[$base]"
    case BaseIndex(base, index, scale) => s"[$base + $index * $scale]"
    case BaseDis(base, dis, label) => s"[$base${formatDisSuffix(dis, label)}]"
    case IndexDis(index, scale, dis, label) => s"[$index * $scale${formatDisSuffix(dis, label)}]"
    case BaseIndexDis(base, index, scale, dis, label) => s"[$base + $index * $scale${formatDisSuffix(dis, label)}]"
  }
  private def formatDis(dis: Int, label: Option[String]): String = label match {
    case Some(label) if dis > 0 => s"$label + $dis"
    case Some(label) if dis < 0 => s"$label - ${-dis}"
    case Some(label) => s"$label"
    case None => s"$dis"
  }
  private def formatDisSuffix(dis: Int, label: Option[String]): String = label match {
    case Some(label) if dis > 0 => s" + $label + $dis"
    case Some(label) if dis < 0 => s" + $label - ${-dis}"
    case Some(label) => s" + $label"
    case None if dis > 0 => s" + $dis"
    case None if dis < 0 => s" - ${-dis}"
    case None => ""
  }
  def +(int: Int): Address = this match {
    case dis: Dis => dis + int
    case base: Base => base + int
    case baseIndex: BaseIndex => baseIndex + int
    case baseDis: BaseDis => baseDis + int
    case indexDis: IndexDis => indexDis + int
    case baseIndexDis: BaseIndexDis => baseIndexDis + int
  }
}

case class Dis(dis: Int, label: Option[String]) extends Address {
  def +(other: Dis): Dis = Dis(dis + other.dis, label.orElse(other.label))
  override def +(int: Int): Dis = Dis(dis + int, label)
}

case class Base(base: Reg) extends Address {
  def +(other: Index): BaseIndex = BaseIndex(base, other.index, other.scale)
  def +(reg: Reg): BaseIndex = BaseIndex(base, reg, 1)
  def +(other: Dis): BaseDis = BaseDis(base, other.dis, other.label)
  override def +(int: Int): BaseDis = BaseDis(base, int, None)
  def +(string: String): BaseDis = BaseDis(base, 0, Some(string))
}

case class BaseIndex(base: Reg, index: Reg, scale: Int) extends Address {
  def +(other: Dis): BaseIndexDis = BaseIndexDis(base, index, scale, other.dis, other.label)
  override def +(int: Int): BaseIndexDis = BaseIndexDis(base, index, scale, int, None)
  def +(string: String): BaseIndexDis = BaseIndexDis(base, index, scale, 0, Some(string))
}

case class BaseDis(base: Reg, dis: Int, label: Option[String]) extends Address {
  def +(other: Dis): BaseDis = BaseDis(base, dis + other.dis, label.orElse(other.label))
  override def +(int: Int): BaseDis = BaseDis(base, dis + int, label)
  def +(string: String): BaseDis = BaseDis(base, dis, Some(string))
}

case class IndexDis(index: Reg, scale: Int, dis: Int, label: Option[String]) extends Address {
  def +(other: Dis): IndexDis = IndexDis(index, scale, dis + other.dis, label.orElse(other.label))
  override def +(int: Int): IndexDis = IndexDis(index, scale, dis + int, label)
  def +(string: String): IndexDis = IndexDis(index, scale, dis, Some(string))
}

case class BaseIndexDis(base: Reg, index: Reg, scale: Int, dis: Int, label: Option[String]) extends Address {
  def +(other: Dis): BaseIndexDis = BaseIndexDis(base, index, scale, dis + other.dis, label.orElse(other.label))
  override def +(int: Int): BaseIndexDis = BaseIndexDis(base, index, scale, dis + int, label)
  def +(string: String): BaseIndexDis = BaseIndexDis(base, index, scale, dis, Some(string))
}

case class Index(index: Reg, scale: Int = 1) {
  assert(scale == 1 || scale == 2 || scale == 4 || scale == 8, "Scale must equal 1, 2, 4, or 8")

  def *(int: Int): Index = Index(index, scale * int)
  def +(other: Dis): IndexDis = IndexDis(index, scale, other.dis, other.label)
  def +(int: Int): IndexDis = IndexDis(index, scale, int, None)
  def +(string: String): IndexDis = IndexDis(index, scale, 0, Some(string))
}

object Address {
  def apply(dis: Int): Dis = Dis(dis, None)
  def apply(label: String): Dis = Dis(0, Some(label))
  def apply(dis: Int, label: String): Dis = Dis(dis, Some(label))
  def apply(reg: Reg): Base = Base(reg)
}

enum Reg(val qword: String, val dword: String, val word: String, val byte: String) {
  case RAX extends Reg("rax", "eax", "ax", "al")
  case RBX extends Reg("rbx", "ebx", "bx", "bl")
  case RCX extends Reg("rcx", "ecx", "cx", "cl")
  case RDX extends Reg("rdx", "edx", "dx", "dl")
  case RSI extends Reg("rsi", "esi", "si", "sil")
  case RDI extends Reg("rdi", "edi", "di", "dil")
  case RBP extends Reg("rbp", "ebp", "bp", "bpl")
  case RSP extends Reg("rsp", "esp", "sp", "spl")
  case R8 extends Reg("r8", "r8d", "r8w", "r8b")
  case R9 extends Reg("r9", "r9d", "r9w", "r9b")
  case R10 extends Reg("r10", "r10d", "r10w", "r10b")
  case R11 extends Reg("r11", "r11d", "r11w", "r11b")
  case R12 extends Reg("r12", "r12d", "r12w", "r12b")
  case R13 extends Reg("r13", "r13d", "r13w", "r13b")
  case R14 extends Reg("r14", "r14d", "r14w", "r14b")
  case R15 extends Reg("r15", "r15d", "r15w", "r15b")

  def apply(regSize: RegSize): String = regSize match {
    case RegSize.Byte => byte
    case RegSize.Word => word
    case RegSize.DWord => dword
    case RegSize.QWord => qword
  }

  override def toString: String = qword

  def *(int: Int): Index = Index(this, int)
  def +(other: Index): BaseIndex = BaseIndex(this, other.index, other.scale)
  def +(reg: Reg): BaseIndex = BaseIndex(this, reg, 1)
  def +(int: Int): BaseDis = BaseDis(this, int, None)
  def +(string: String): BaseDis = BaseDis(this, 0, Some(string))
}

enum RegSize(val size: Int, val string: String) {
  case Byte extends RegSize(1, "byte")
  case Word extends RegSize(2, "word")
  case DWord extends RegSize(4, "dword")
  case QWord extends RegSize(8, "qword")

  override def toString: String = string
}

enum Flag(val name: String) {
  case Overflow extends Flag("o")
  case NotOverflow extends Flag("no")
  case Sign extends Flag("s")
  case NotSign extends Flag("ns")
  case Zero extends Flag("z")
  case NotZero extends Flag("nz")
  case Below extends Flag("b")
  case AboveOrEqual extends Flag("ae")
  case BelowOrEqual extends Flag("be")
  case Above extends Flag("a")
  case Less extends Flag("l")
  case GreaterOrEqual extends Flag("ge")
  case LessOrEqual extends Flag("le")
  case Greater extends Flag("g")
  case Parity extends Flag("p")
  case NotParity extends Flag("np")
}

abstract class SimpleOp(val name: String) {
  def apply(dst: Reg, src: Reg): SimpleOpReg = SimpleOpReg(this, dst, src, None)
  def apply(dst: Reg, src: Int): SimpleOpImm = SimpleOpImm(this, dst, src, None)
  def apply(dst: Reg, src: Address): SimpleOpAddress = SimpleOpAddress(this, dst, src, None)
  def apply(dst: Address, src: Reg): SimpleOpMem = SimpleOpMem(this, dst, src, None)
  def apply(dst: Address, src: Int): SimpleOpMemImm = SimpleOpMemImm(this, dst, src, None)
}

case object Add extends SimpleOp("add")

case object Sub extends SimpleOp("sub")

case object And extends SimpleOp("and")

case object Or extends SimpleOp("or")

case object Xor extends SimpleOp("xor")

case object Cmp extends SimpleOp("cmp")

object AsmGen {
  private val DEFINE_BYTE = "db".padTo(8, ' ')
  private val DEFINE_WORD = "dw".padTo(8, ' ')
  private val DEFINE_DWORD = "dd".padTo(8, ' ')
  private val DEFINE_QWORD = "dq".padTo(8, ' ')

  private val bssSection = Section("section '.bss' data readable writeable", 'B')
  private val constSection = Section("section '.const' data readable", 'C')
  private val dataSection = Section("section '.data' data readable writeable", 'D')

  private val labelGen = LabelGen('L')
  private val functionLabelGen = LabelGen('F')

  private val codeBuilder = mutable.Buffer[(String, List[Instr])]()
  private val mainBuilder = mutable.StringBuilder()
  private val kernelTableBuilder = mutable.StringBuilder()
  private val windowsFunctionNamesBuilder = mutable.StringBuilder()

  private val windowsFunctions = mutable.Set[String]()

  def label(): String = labelGen.next()

  def functionLabel(): String = functionLabelGen.next()

  def function(label: String, instructions: List[Instr]): Unit = {
    codeBuilder.append((label, instructions))
  }

  def main(instructions: Instr*): Unit = {
    instructions.foreach(mainBuilder.append)
  }

  def windowsFunction(functionName: String): String = if (windowsFunctions.contains(functionName)) {
    functionName
  } else {
    windowsFunctions.add(functionName)

    kernelTableBuilder.append(s"$functionName:\n")
    kernelTableBuilder.append(" " * 8).append(s"${DEFINE_QWORD}RVA _$functionName\n")

    windowsFunctionNamesBuilder.append(s"_$functionName:\n")
    windowsFunctionNamesBuilder.append(" " * 8).append(s"${DEFINE_BYTE}0, 0, '$functionName', 0\n")
    functionName
  }
  def bss(size: Int, align: Int = 1): (String, String) = bssSection.append(s"$DEFINE_BYTE$size dup(?)", align)

  def constBytes(data: List[Int]): (String, String) = constSection.append(s"$DEFINE_BYTE${data.map(b => s"$b")}", 1)
  def constQWords(data: List[Long]): (String, String) = constSection.append(s"$DEFINE_QWORD${data.map(l => s"$l")}", 8)
  def const(constVal: ConstVal): (String, String) = constSection.append(mapConstVal(constVal), constVal.datatype.align)

  def dataBytes(data: List[Int]): (String, String) = dataSection.append(s"$DEFINE_BYTE${data.map(b => s"$b")}", 1)
  def dataQWords(data: List[Long]): (String, String) = dataSection.append(s"$DEFINE_QWORD${data.map(l => s"$l")}", 8)
  def data(constVal: ConstVal): (String, String) = dataSection.append(mapConstVal(constVal), constVal.datatype.align)

  private def mapConstVal(constVal: ConstVal): String = constVal match {
    case ConstUnit => ""
    case ConstInt(int) => s"$DEFINE_QWORD$int\n"
    case ConstBool(bool) => s"$DEFINE_BYTE$bool\n"
    case ConstTuple(elements) => elements.map(mapConstVal).mkString
    case ConstFunction(function) => s"$DEFINE_QWORD${function.label}\n"
  } match {
    case "" => ""
    case string => s"align ${constVal.datatype.align}\n" + string
  }

  override def toString: String = mutable.StringBuilder()
    .append("format pe64 console\nentry start\n\n")
    .append(s"section '.text' code readable executable\n\n${codeBuilder.map { (label, instr) => s"$label:\n${instr.mkString}" }.mkString("\n")}start:\n$mainBuilder\n")
    .append(s"$bssSection")
    .append(s"$constSection")
    .append(s"$dataSection")
    .append(s"section '.idata' import data readable writeable\n\n")
    .append(" " * 8).append(s"${DEFINE_DWORD}0, 0, 0, RVA kernel_name, RVA kernel_table, 0, 0, 0, 0, 0\n\n")
    .append(s"kernel_table:\n$kernelTableBuilder${" " * 8}${DEFINE_QWORD}0\n\n")
    .append(s"kernel_name:\n${" " * 8}$DEFINE_BYTE'KERNEL32.DLL', 0\n")
    .append(s"user_name:\n${" " * 8}$DEFINE_BYTE'USER32.DLL', 0\n\n")
    .append(windowsFunctionNamesBuilder)
    .toString

  class LabelGen(labelChar: Char) {
    var count = 0

    def next(): String = {
      val label = s"$labelChar$count"
      count += 1
      label
    }
  }

  class Section(initial: String, labelChar: Char) {
    val builder: mutable.StringBuilder = mutable.StringBuilder(s"$initial\n\n${labelChar}0:\n")
    var count: Int = 0

    def append(string: String, align: Int): (String, String) = {
      if (align > 1)
        builder.append(s"align $align\n")
      val beforeLabel = s"$labelChar$count"
      count += 1
      val afterLabel = s"$labelChar$count"
      builder.append(" " * 8).append(string).append('\n')
      builder.append(s"$labelChar$count:\n")
      (beforeLabel, afterLabel)
    }

    override def toString: String = if count > 0 then s"$builder\n" else ""
  }
}
