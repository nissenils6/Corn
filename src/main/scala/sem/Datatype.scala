package sem

import core.*
import gen.*

import scala.annotation.targetName
import scala.collection.mutable

abstract class Datatype {
  def mutable: Boolean

  lazy val (size: Int, align: Int) = this match {
    case UnitDatatype(_) => (0, 0)
    case IntDatatype(_) => (8, 8)
    case BoolDatatype(_) => (1, 1)
    case TypeDatatype(_) => (0, 0)
    case TupleDatatype(elements, _) =>
      val (size, align, _) = Datatype.alignSequence(elements)
      (size, align)
    case FunDatatype(_, _, _) => (8, 8)
  }

  lazy val (runtime: Boolean, compiletime: Boolean) = this match {
    case UnitDatatype(_) => (true, true)
    case IntDatatype(_) => (true, true)
    case BoolDatatype(_) => (true, true)
    case TypeDatatype(_) => (false, true)
    case TupleDatatype(elements, _) => (elements.forall(_.runtime), elements.forall(_.compiletime))
    case FunDatatype(_, _, _) => (true, true)
  }
  
  @targetName("implicitCast")
  def ~=(datatype: Datatype): Boolean = (this, datatype) match {
    case (TupleDatatype(elements1, _), TupleDatatype(elements2, _)) => elements1.zip(elements2).forall{case (d1, d2) => d1 ~= d2}
    case (FunDatatype(_, _, _), FunDatatype(_, _, _)) => this == datatype
    case (_, _) => getClass == datatype.getClass
  }
  
  def !~=(datatype: Datatype): Boolean = !(this ~= datatype)
  
  def withMut(mutable: Boolean): Datatype = this match {
    case UnitDatatype(_) => UnitDatatype(mutable)
    case IntDatatype(_) => IntDatatype(mutable)
    case BoolDatatype(_) => BoolDatatype(mutable)
    case TypeDatatype(_) => TypeDatatype(mutable)
    case TupleDatatype(elements, _) => TupleDatatype(elements, mutable)
    case FunDatatype(params, returnType, _) => FunDatatype(params, returnType, mutable)
  }

  def asUnit: Option[UnitDatatype] = if isInstanceOf[UnitDatatype] then Some(asInstanceOf[UnitDatatype]) else None
  def asInt: Option[IntDatatype] = if isInstanceOf[IntDatatype] then Some(asInstanceOf[IntDatatype]) else None
  def asBool: Option[BoolDatatype] = if isInstanceOf[BoolDatatype] then Some(asInstanceOf[BoolDatatype]) else None
  def asType: Option[TypeDatatype] = if isInstanceOf[TypeDatatype] then Some(asInstanceOf[TypeDatatype]) else None
  def asTuple: Option[TupleDatatype] = if isInstanceOf[TupleDatatype] then Some(asInstanceOf[TupleDatatype]) else None
  def asFun: Option[FunDatatype] = if isInstanceOf[FunDatatype] then Some(asInstanceOf[FunDatatype]) else None

  def generateCopyCode(ctx: ExprCodeGenContext, dst: Address, src: Address): Unit = this match {
    case UnitDatatype(_) => ()
    case IntDatatype(_) | FunDatatype(_, _, _) => ctx.add(
      Load(Reg.RAX, src),
      Store(dst, Reg.RAX)
    )
    case BoolDatatype(_) => ctx.add(
      Load(Reg.RAX, src, RegSize.Byte),
      Store(dst, Reg.RAX, RegSize.Byte)
    )
    case TupleDatatype(elements, _) => elements.zip(Datatype.alignSequence(elements)._3).foreach(t => t._1.generateCopyCode(ctx, dst + t._2, src + t._2))
  }

  override def toString: String = (if mutable then "mut " else "") + (this match {
    case UnitDatatype(_) => s"()"
    case IntDatatype(_) => s"Int"
    case BoolDatatype(_) => s"Bool"
    case TypeDatatype(_) => s"Type"
    case TupleDatatype(elements, _) => s"(${elements.mkString(", ")})"
    case FunDatatype(params, returnType, _) => s"((${params.mkString(", ")}) => $returnType)"
  })
}

case class UnitDatatype(mutable: Boolean) extends Datatype

case class IntDatatype(mutable: Boolean) extends Datatype

case class BoolDatatype(mutable: Boolean) extends Datatype

case class TypeDatatype(mutable: Boolean) extends Datatype

case class TupleDatatype(elements: List[Datatype], mutable: Boolean) extends Datatype

case class FunDatatype(params: List[Datatype], returnType: Datatype, mutable: Boolean) extends Datatype

object Datatype {
  def alignSequence(elements: List[Datatype]): (Int, Int, List[Int]) = {
    var size = 0
    var align = 1
    val offsets = mutable.Buffer[Int]()
    for (element <- elements) {
      size = size.roundUp(element.align)
      offsets.append(size)
      size += element.size
      align = align.max(element.align)
    }
    (size, align, offsets.toList)
  }
}

abstract class ConstVal {
  lazy val datatype: Datatype = this match {
    case ConstUnit => UnitDatatype(false)
    case ConstInt(_) => IntDatatype(false)
    case ConstBool(_) => BoolDatatype(false)
    case ConstType(_) => TypeDatatype(false)
    case ConstTuple(elements) => TupleDatatype(elements.map(_.datatype), false)
    case ConstFunction(function) => function.signature
  }

  lazy val toType: Option[Datatype] = this match {
    case ConstType(datatype) => Some(datatype)
    case ConstTuple(elements) if elements.map(_.toType).forall(_.nonEmpty) => Some(TupleDatatype(elements.map(_.toType.get), false))
    case ConstUnit => Some(UnitDatatype(false))
    case _ => None
  }

  def toInt: Long = this.asInt.get.int
  def toBool: Boolean = this.asBool.get.bool

  def asInt: Option[ConstInt] = if isInstanceOf[ConstInt] then Some(asInstanceOf[ConstInt]) else None
  def asBool: Option[ConstBool] = if isInstanceOf[ConstBool] then Some(asInstanceOf[ConstBool]) else None
  def asType: Option[ConstType] = if isInstanceOf[ConstType] then Some(asInstanceOf[ConstType]) else None
  def asTuple: Option[ConstTuple] = if isInstanceOf[ConstTuple] then Some(asInstanceOf[ConstTuple]) else None
  def asFun: Option[ConstFunction] = if isInstanceOf[ConstFunction] then Some(asInstanceOf[ConstFunction]) else None

  def generateCode(address: Address): List[Instr]

  override def toString: String = this match {
    case ConstUnit => "()"
    case ConstInt(int) => s"$int"
    case ConstBool(bool) => s"$bool"
    case ConstType(datatype) => s"$datatype"
    case ConstTuple(elements) => s"(${elements.mkString(", ")})"
    case ConstFunction(_) => s"<FUNCTION>"
  }
}

case object ConstUnit extends ConstVal {
  override def generateCode(address: Address): List[Instr] = List()
}

case class ConstInt(int: Long) extends ConstVal {
  override def generateCode(address: Address): List[Instr] = if (int.toInt == int) List(
    StoreImm(address, int.toInt)
  ) else List(
    LoadImm(Reg.RAX, int),
    Store(address, Reg.RAX)
  )
}

case class ConstBool(bool: Boolean) extends ConstVal {
  override def generateCode(address: Address): List[Instr] = List(
    StoreImm(address, if bool then 1 else 0, RegSize.Byte)
  )
}

case class ConstType(valDatatype: Datatype) extends ConstVal {
  override def generateCode(address: Address): List[Instr] = List()
}

case class ConstTuple(elements: List[ConstVal]) extends ConstVal {
  override def generateCode(address: Address): List[Instr] =
    elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3).flatMap { case (element, offset) => element.generateCode(address + offset) }
}

case class ConstFunction(function: Fun) extends ConstVal {
  override def generateCode(address: Address): List[Instr] = List(
    Lea(Reg.RAX, Address(function.label)),
    Store(address, Reg.RAX)
  )
}
