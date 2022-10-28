abstract class Datatype {
  lazy val (size: Int, align: Int) = this match {
    case UnitDatatype => (0, 0)
    case IntDatatype => (8, 8)
    case TypeDatatype => (0, 0)
    case TupleDatatype(elements) => (elements.map(_.size).sum, elements.map(_.align).max)
    case FunDatatype(_, _) => (8, 8)
  }

  lazy val (runtime: Boolean, compiletime: Boolean) = this match {
    case UnitDatatype => (true, true)
    case IntDatatype => (true, true)
    case TypeDatatype => (false, true)
    case TupleDatatype(elements) => (elements.forall(_.runtime), elements.forall(_.compiletime))
    case FunDatatype(_, _) => (true, true)
  }

  override def toString: String = this match {
    case UnitDatatype => s"()"
    case IntDatatype => s"Int"
    case TypeDatatype => s"Type"
    case TupleDatatype(elements) => s"(${elements.mkString(", ")})"
    case FunDatatype(params, returnType) => s"(${params.mkString(", ")}) => $returnType"
  }
}

case object UnitDatatype extends Datatype

case object IntDatatype extends Datatype

case object TypeDatatype extends Datatype

case class TupleDatatype(elements: List[Datatype]) extends Datatype

case class FunDatatype(params: List[Datatype], returnType: Datatype) extends Datatype

abstract class ConstVal {
  lazy val datatype: Datatype = this match {
    case ConstUnit => UnitDatatype
    case ConstInt(_) => IntDatatype
    case ConstType(_) => TypeDatatype
    case ConstTuple(elements) => TupleDatatype(elements.map(_.datatype))
    case ConstFunction(function) => function.signature
  }

  def toInt: Int = asInstanceOf[ConstInt].int

  override def toString: String = this match {
    case ConstUnit => "()"
    case ConstInt(int) => s"$int"
    case ConstType(datatype) => s"$datatype"
    case ConstTuple(elements) => s"(${elements.mkString(", ")})"
    case ConstFunction(function) => s"${function.name}"
  }
}

case object ConstUnit extends ConstVal

case class ConstInt(int: Int) extends ConstVal

case class ConstType(valDatatype: Datatype) extends ConstVal

case class ConstTuple(elements: List[ConstVal]) extends ConstVal

case class ConstFunction(function: GlobalFun) extends ConstVal
