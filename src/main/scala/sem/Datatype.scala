package sem

import core.*

import scala.annotation.targetName
import scala.collection.mutable

abstract class Datatype {
  def mutable: Boolean

  lazy val (size: Int, align: Int) = this match {
    case UnitDatatype(_) => (0, 0)
    case IntDatatype(_) | RefDatatype(_, _) | FunDatatype(_, _, _) => (8, 8)
    case BoolDatatype(_) => (1, 1)
    case TypeDatatype(_) => (0, 0)
    case TupleDatatype(elements, _) =>
      val (size, align, _) = Datatype.alignSequence(elements)
      (size, align)
  }

  lazy val runtime: Boolean = this match {
    case UnitDatatype(_) | IntDatatype(_) | BoolDatatype(_) | RefDatatype(_, _) => true
    case TypeDatatype(_) => false
    case TupleDatatype(elements, _) => elements.forall(_.runtime)
    case FunDatatype(params, returnType, _) => returnType.runtime && params.forall(_.runtime)
  }

  lazy val optDatatype: opt.Datatype = this match {
    case UnitDatatype(_) => opt.UnitDatatype
    case IntDatatype(_) => opt.IntDatatype
    case BoolDatatype(_) => opt.BoolDatatype
    case RefDatatype(datatype, _) => opt.RefDatatype(datatype.optDatatype)
    case TupleDatatype(elements, _) => opt.TupleDatatype(elements.map(_.optDatatype))
    case FunDatatype(params, returnType, _) => opt.FunDatatype(params.map(_.optDatatype), List(returnType.optDatatype))
  }

  @targetName("implicitCast")
  def ~=>(datatype: Datatype): Boolean = (this, datatype) match {
    case (RefDatatype(datatype1, _), RefDatatype(datatype2, _)) => (datatype1 ~=> datatype2) && (!datatype2.mutable || datatype1.mutable)
    case (TupleDatatype(elements1, _), TupleDatatype(elements2, _)) => elements1.zip(elements2).forall { (d1, d2) => d1 ~=> d2 }
    case (FunDatatype(_, _, _), FunDatatype(_, _, _)) => this == datatype
    case (_, _) => getClass == datatype.getClass
  }

  @targetName("notImplicitCast")
  def !~=>(datatype: Datatype): Boolean = !(this ~=> datatype)

  def withMut(mutable: Boolean): Datatype = this match {
    case UnitDatatype(_) => UnitDatatype(mutable)
    case IntDatatype(_) => IntDatatype(mutable)
    case BoolDatatype(_) => BoolDatatype(mutable)
    case TypeDatatype(_) => TypeDatatype(mutable)
    case RefDatatype(datatype, _) => RefDatatype(datatype, mutable)
    case TupleDatatype(elements, _) => TupleDatatype(elements, mutable)
    case FunDatatype(params, returnType, _) => FunDatatype(params, returnType, mutable)
  }

  override def toString: String = (if mutable then "mut " else "") + (this match {
    case UnitDatatype(_) => s"()"
    case IntDatatype(_) => s"Int"
    case BoolDatatype(_) => s"Bool"
    case TypeDatatype(_) => s"Type"
    case RefDatatype(datatype, _) => s"ref $datatype"
    case TupleDatatype(elements, _) => s"(${elements.mkString(", ")})"
    case FunDatatype(params, returnType, _) => s"((${params.mkString(", ")}) => $returnType)"
  })
}

case class UnitDatatype(mutable: Boolean) extends Datatype
case class IntDatatype(mutable: Boolean) extends Datatype
case class BoolDatatype(mutable: Boolean) extends Datatype
case class TypeDatatype(mutable: Boolean) extends Datatype
case class RefDatatype(datatype: Datatype, mutable: Boolean) extends Datatype
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
    case ConstRef(VarRefBox(globalVar: GlobalVar)) => for {
      constVal <- globalVar.constVal
      constType <- constVal.toType
    } yield RefDatatype(constType, false)
    case ConstRef(VarRefBox(localVar: LocalVar)) => for {
      constVal <- localVar.constVal
      constType <- constVal.toType
    } yield RefDatatype(constType, false)
    case ConstTuple(elements) => elements.map(_.toType).extract.map(elements => TupleDatatype(elements, false))
    case ConstUnit => Some(UnitDatatype(false))
    case _ => None
  }

  def generateIr(funsMap: Map[Fun, opt.Fun]): opt.ConstVal = this match {
    case ConstUnit => opt.ConstUnit
    case ConstInt(int) => opt.ConstInt(int)
    case ConstBool(bool) => opt.ConstBool(bool)
    case ConstRef(_) => ???
    case ConstTuple(elements) => opt.ConstTuple(elements.map(_.generateIr(funsMap)))
    case ConstFunction(fun) => opt.ConstFun(funsMap(fun))
  }

  def toInt: Long = this.asInt.get.int
  def toBool: Boolean = this.asBool.get.bool

  def asInt: Option[ConstInt] = if isInstanceOf[ConstInt] then Some(asInstanceOf[ConstInt]) else None
  def asBool: Option[ConstBool] = if isInstanceOf[ConstBool] then Some(asInstanceOf[ConstBool]) else None
  def asType: Option[ConstType] = if isInstanceOf[ConstType] then Some(asInstanceOf[ConstType]) else None
  def asRef: Option[ConstRef] = if isInstanceOf[ConstRef] then Some(asInstanceOf[ConstRef]) else None
  def asTuple: Option[ConstTuple] = if isInstanceOf[ConstTuple] then Some(asInstanceOf[ConstTuple]) else None
  def asFun: Option[ConstFunction] = if isInstanceOf[ConstFunction] then Some(asInstanceOf[ConstFunction]) else None

  def gatherFuns(funs: mutable.Set[Fun]): Unit = this match {
    case ConstTuple(elements) => elements.foreach(_.gatherFuns(funs))
    case ConstFunction(function) => funs.add(function)
    case _ => ()
  }

  override def toString: String = this match {
    case ConstUnit => "()"
    case ConstInt(int) => s"$int"
    case ConstBool(bool) => s"$bool"
    case ConstType(datatype) => s"$datatype"
    case ConstRef(VarRefBox(localVar: LocalVar)) => s"ref ${localVar.name}"
    case ConstRef(VarRefBox(globalVar: GlobalVar)) => s"ref ${globalVar.name}"
    case ConstTuple(elements) => s"(${elements.mkString(", ")})"
    case ConstFunction(function) => s"fn(${function.argTypes.mkString(", ")}) => ${function.returnType}"
  }
}

case object ConstUnit extends ConstVal
case class ConstInt(int: Long) extends ConstVal
case class ConstBool(bool: Boolean) extends ConstVal
case class ConstType(valDatatype: Datatype) extends ConstVal
case class ConstRef(box: RefBox) extends ConstVal
case class ConstTuple(elements: List[ConstVal]) extends ConstVal
case class ConstFunction(function: Fun) extends ConstVal

abstract class RefBox

case class VarRefBox(variable: Var) extends RefBox
