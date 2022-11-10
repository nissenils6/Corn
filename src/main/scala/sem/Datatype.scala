package sem

import core.*
import gen.*

import scala.collection.mutable

abstract class Datatype {
  lazy val (size: Int, align: Int) = this match {
    case UnitDatatype => (0, 0)
    case IntDatatype => (8, 8)
    case BoolDatatype => (1, 1)
    case TypeDatatype => (0, 0)
    case TupleDatatype(elements) => 
      val (size, align, _) = Datatype.alignSequence(elements)
      (size, align)
    case FunDatatype(_, _) => (8, 8)
  }

  lazy val (runtime: Boolean, compiletime: Boolean) = this match {
    case UnitDatatype => (true, true)
    case IntDatatype => (true, true)
    case BoolDatatype => (true, true)
    case TypeDatatype => (false, true)
    case TupleDatatype(elements) => (elements.forall(_.runtime), elements.forall(_.compiletime))
    case FunDatatype(_, _) => (true, true)
  }

  def generateCopyCode(ctx: ExprCodeGenContext, dst: Address, src: Address): Unit = this match {
    case UnitDatatype => ()
    case IntDatatype | FunDatatype(_, _) => ctx.add(
      Load(Reg.RAX, src),
      Store(dst, Reg.RAX)
    )
    case BoolDatatype => ctx.add(
      Load(Reg.RAX, src, RegSize.Byte),
      Store(dst, Reg.RAX, RegSize.Byte)
    )
    case TupleDatatype(elements) => elements.zip(Datatype.alignSequence(elements)._3).foreach(t => t._1.generateCopyCode(ctx, dst + t._2, src + t._2))
  }

  override def toString: String = this match {
    case UnitDatatype => s"()"
    case IntDatatype => s"Int"
    case BoolDatatype => s"Bool"
    case TypeDatatype => s"Type"
    case TupleDatatype(elements) => s"(${elements.mkString(", ")})"
    case FunDatatype(params, returnType) => s"((${params.mkString(", ")}) => $returnType)"
  }
}

case object UnitDatatype extends Datatype

case object IntDatatype extends Datatype

case object BoolDatatype extends Datatype

case object TypeDatatype extends Datatype

case class TupleDatatype(elements: List[Datatype]) extends Datatype

case class FunDatatype(params: List[Datatype], returnType: Datatype) extends Datatype

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
    case ConstUnit => UnitDatatype
    case ConstInt(_) => IntDatatype
    case ConstBool(_) => BoolDatatype
    case ConstType(_) => TypeDatatype
    case ConstTuple(elements) => TupleDatatype(elements.map(_.datatype))
    case ConstFunction(function) => function.signature
  }

  lazy val asType: Option[Datatype] = this match {
    case ConstType(datatype) => Some(datatype)
    case ConstTuple(elements) if elements.map(_.asType).forall(_.nonEmpty) => Some(TupleDatatype(elements.map(_.asType.get)))
    case ConstUnit => Some(UnitDatatype)
    case _ => None
  }

  def toInt: Long = asInstanceOf[ConstInt].int
  def toBool: Boolean = asInstanceOf[ConstBool].bool

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
