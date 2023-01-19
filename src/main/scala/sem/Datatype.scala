package sem

import core.*
import syn.*

import scala.annotation.targetName
import scala.collection.mutable

abstract class Datatype {
  def mutable: Boolean

  lazy val optDatatype: opt.Datatype = this match {
    case UnitDatatype => opt.UnitDatatype
    case IntDatatype | MutIntDatatype => opt.IntDatatype
    case BoolDatatype | MutBoolDatatype => opt.BoolDatatype
    case RefDatatype(datatype, _) => opt.RefDatatype(datatype.optDatatype)
    case TupleDatatype(elements, _) => opt.TupleDatatype(elements.map(_.optDatatype))
    case FunDatatype(params, returnType, _) => opt.FunDatatype(params.map(_.optDatatype), List(returnType.optDatatype))
  }

  def isSubtypeOf(datatype: Datatype): Boolean = (this, datatype) match {
    case (UnitDatatype, UnitDatatype) => true
    case (IntDatatype | MutIntDatatype, IntDatatype | MutIntDatatype) => !datatype.mutable || mutable
    case (BoolDatatype | MutBoolDatatype, BoolDatatype | MutBoolDatatype) => !datatype.mutable || mutable
    case (RefDatatype(datatype1, mutable1), RefDatatype(datatype2, mutable2)) => datatype1.isSubtypeOf(datatype2) && (!mutable2 || mutable1)
    case (TupleDatatype(elements1, _), TupleDatatype(elements2, _)) => elements1.zip(elements2).forall { case (e1, e2) => e1.isSubtypeOf(e2) }
    case (FunDatatype(params1, returnType1, _), FunDatatype(params2, returnType2, _)) => params1.zip(params2).forall { case (p1, p2) => p2.isSubtypeOf(p1) } && returnType1.isSubtypeOf(returnType2)
    case _ => false
  }
  
  def withMut(mutable: Boolean): Datatype = this match {
    case UnitDatatype => UnitDatatype
    case IntDatatype | MutIntDatatype if !mutable => IntDatatype
    case IntDatatype | MutIntDatatype if mutable => MutIntDatatype
    case BoolDatatype | MutBoolDatatype if !mutable => BoolDatatype
    case BoolDatatype | MutBoolDatatype if mutable => MutBoolDatatype
    case RefDatatype(datatype, _) => RefDatatype(datatype, mutable)
    case TupleDatatype(elements, _) => TupleDatatype(elements, mutable)
    case FunDatatype(params, returnType, _) => FunDatatype(params, returnType, mutable)
  }

  override def toString: String = (if mutable then "mut " else "") + (this match {
    case UnitDatatype => s"()"
    case IntDatatype | MutIntDatatype => "$Int"
    case BoolDatatype | MutBoolDatatype => "$Bool"
    case RefDatatype(datatype, _) => s"@$datatype"
    case TupleDatatype(elements, _) => s"(${elements.mkString(", ")})"
    case FunDatatype(params, returnType, _) => s"(${params.mkString(", ")}) => $returnType"
  })
}

case object UnitDatatype extends Datatype {
  override def mutable: Boolean = false
}

case object IntDatatype extends Datatype {
  override def mutable: Boolean = false
}

case object MutIntDatatype extends Datatype {
  override def mutable: Boolean = true
}

case object BoolDatatype extends Datatype{
  override def mutable: Boolean = false
}

case object MutBoolDatatype extends Datatype{
  override def mutable: Boolean = true
}

case class RefDatatype(datatype: Datatype, mutable: Boolean) extends Datatype
case class TupleDatatype(elements: List[Datatype], mutable: Boolean) extends Datatype
case class FunDatatype(params: List[Datatype], returnType: Datatype, mutable: Boolean) extends Datatype

abstract class ConstVal {
  lazy val datatype: Datatype = this match {
    case ConstUnit => UnitDatatype
    case ConstInt(_) => IntDatatype
    case ConstBool(_) => BoolDatatype
    case ConstTuple(elements) => TupleDatatype(elements.map(_.datatype), false)
    case ConstFun(fun) => fun.signature.get
  }

//  lazy val toType: Option[Datatype] = this match {
//    case ConstType(datatype) => Some(datatype)
//    case ConstTuple(elements) => elements.map(_.toType).extract.map(elements => TupleDatatype(elements, false))
//    case ConstUnit => Some(UnitDatatype(false))
//    case _ => None
//  }

//  def generateIr(funsMap: Map[Fun, opt.Fun]): opt.ConstVal = this match {
//    case ConstUnit => opt.ConstUnit
//    case ConstInt(int) => opt.ConstInt(int)
//    case ConstBool(bool) => opt.ConstBool(bool)
//    case ConstTuple(elements) => opt.ConstTuple(elements.map(_.generateIr(funsMap)))
//    case ConstFun(fun) => opt.ConstFun(funsMap(fun))
//  }

  def toInt: Long = this.asInt.get.int
  def toBool: Boolean = this.asBool.get.bool

  def asInt: Option[ConstInt] = if isInstanceOf[ConstInt] then Some(asInstanceOf[ConstInt]) else None
  def asBool: Option[ConstBool] = if isInstanceOf[ConstBool] then Some(asInstanceOf[ConstBool]) else None
  def asTuple: Option[ConstTuple] = if isInstanceOf[ConstTuple] then Some(asInstanceOf[ConstTuple]) else None
  def asFun: Option[ConstFun] = if isInstanceOf[ConstFun] then Some(asInstanceOf[ConstFun]) else None

//  def gatherFuns(funs: mutable.Set[Fun]): Unit = this match {
//    case ConstTuple(elements) => elements.foreach(_.gatherFuns(funs))
//    case ConstFun(function) => funs.add(function)
//    case _ => ()
//  }

  override def toString: String = this match {
    case ConstUnit => "()"
    case ConstInt(int) => s"$int"
    case ConstBool(bool) => s"$bool"
    case ConstTuple(elements) => s"(${elements.mkString(", ")})"
    case ConstFun(fun) => s"(${fun.signature.get.params.mkString(", ")}) => ${fun.signature.get.returnType}"
  }
}

case object ConstUnit extends ConstVal
case class ConstInt(int: Long) extends ConstVal
case class ConstBool(bool: Boolean) extends ConstVal
case class ConstTuple(elements: List[ConstVal]) extends ConstVal
case class ConstFun(fun: Fun) extends ConstVal
