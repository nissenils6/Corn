import Datatype.alignSequence

import scala.collection.mutable

abstract class Datatype {
  lazy val (size: Int, align: Int) = this match {
    case UnitDatatype => (0, 0)
    case IntDatatype => (8, 8)
    case TypeDatatype => (0, 0)
    case TupleDatatype(elements) => {
      val (size, align, _) = Datatype.alignSequence(elements)
      (size, align)
    }
    case FunDatatype(_, _) => (8, 8)
  }

  lazy val (runtime: Boolean, compiletime: Boolean) = this match {
    case UnitDatatype => (true, true)
    case IntDatatype => (true, true)
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
    case TupleDatatype(elements) => elements.zip(alignSequence(elements)._3).foreach(t => t._1.generateCopyCode(ctx, dst + t._2, src + t._2))
  }

  override def toString: String = this match {
    case UnitDatatype => s"()"
    case IntDatatype => s"Int"
    case TypeDatatype => s"Type"
    case TupleDatatype(elements) => s"(${elements.mkString(", ")})"
    case FunDatatype(params, returnType) => s"((${params.mkString(", ")}) => $returnType)"
  }
}

case object UnitDatatype extends Datatype

case object IntDatatype extends Datatype

case object TypeDatatype extends Datatype

case class TupleDatatype(elements: List[Datatype]) extends Datatype

case class FunDatatype(params: List[Datatype], returnType: Datatype) extends Datatype {
  lazy val (argSize: Int, argAlignment: Int, argOffsets: List[Int]) = {
    val (size, align, offsets) = alignSequence(params)
    (size, align, offsets.zip(params).map(t => size - t._1 - t._2.size))
  }
}

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
    case ConstType(_) => TypeDatatype
    case ConstTuple(elements) => TupleDatatype(elements.map(_.datatype))
    case ConstFunction(function) => function.signature
  }

  def toInt: Int = asInstanceOf[ConstInt].int

  def generateCode(address: Address): List[Instr]

  override def toString: String = this match {
    case ConstUnit => "()"
    case ConstInt(int) => s"$int"
    case ConstType(datatype) => s"$datatype"
    case ConstTuple(elements) => s"(${elements.mkString(", ")})"
    case ConstFunction(function) => s"<FUNCTION>"
  }
}

case object ConstUnit extends ConstVal {
  override def generateCode(address: Address): List[Instr] = List()
}

case class ConstInt(int: Int) extends ConstVal {
  override def generateCode(address: Address): List[Instr] = List(
    StoreImm(address, int)
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
