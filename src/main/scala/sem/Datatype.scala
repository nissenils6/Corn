package sem

import core.*
import syn.FunExpr

import scala.annotation.targetName
import scala.collection.mutable

abstract class Datatype {
  def mutable: Boolean

  lazy val runtime: Boolean = this match {
    case UnitDatatype(_) | IntDatatype(_) | BoolDatatype(_) | RefDatatype(_, _) => true
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

  def isSubtypeOf(datatype: Datatype): Boolean = (this, datatype) match {
    case (RefDatatype(datatype1, _), RefDatatype(datatype2, _)) => (datatype1.isSubtypeOf(datatype2)) && (!datatype2.mutable || datatype1.mutable)
    case (TupleDatatype(elements1, _), TupleDatatype(elements2, _)) => elements1.zip(elements2).forall { (d1, d2) => d1.isSubtypeOf(d2) }
    case (FunDatatype(_, _, _), FunDatatype(_, _, _)) => this == datatype
    case (_, _) => getClass == datatype.getClass
  }
  
  def withMut(mutable: Boolean): Datatype = this match {
    case UnitDatatype(_) => UnitDatatype(mutable)
    case IntDatatype(_) => IntDatatype(mutable)
    case BoolDatatype(_) => BoolDatatype(mutable)
    case RefDatatype(datatype, _) => RefDatatype(datatype, mutable)
    case TupleDatatype(elements, _) => TupleDatatype(elements, mutable)
    case FunDatatype(params, returnType, _) => FunDatatype(params, returnType, mutable)
  }

  override def toString: String = (if mutable then "mut " else "") + (this match {
    case UnitDatatype(_) => s"()"
    case IntDatatype(_) => s"Int"
    case BoolDatatype(_) => s"Bool"
    case RefDatatype(datatype, _) => s"ref $datatype"
    case TupleDatatype(elements, _) => s"(${elements.mkString(", ")})"
    case FunDatatype(params, returnType, _) => s"((${params.mkString(", ")}) => $returnType)"
  })
}

case class UnitDatatype(mutable: Boolean) extends Datatype
case class IntDatatype(mutable: Boolean) extends Datatype
case class BoolDatatype(mutable: Boolean) extends Datatype
case class RefDatatype(datatype: Datatype, mutable: Boolean) extends Datatype
case class TupleDatatype(elements: List[Datatype], mutable: Boolean) extends Datatype
case class FunDatatype(params: List[Datatype], returnType: Datatype, mutable: Boolean) extends Datatype

abstract class ConstVal {
  lazy val datatype: Datatype = this match {
    case ConstUnit => UnitDatatype(false)
    case ConstInt(_) => IntDatatype(false)
    case ConstBool(_) => BoolDatatype(false)
    case ConstTuple(elements) => TupleDatatype(elements.map(_.datatype), false)
    case ConstFun(function) => function.signature
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
    case ConstFun(fun) => s"(${fun.argTypes.mkString(", ")}) => ${fun.returnType}"
  }
}

case object ConstUnit extends ConstVal
case class ConstInt(int: Long) extends ConstVal
case class ConstBool(bool: Boolean) extends ConstVal
case class ConstTuple(elements: List[ConstVal]) extends ConstVal
case class ConstFun(function: FunExpr) extends ConstVal
