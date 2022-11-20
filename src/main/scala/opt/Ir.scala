package opt

abstract class Datatype {

}

case object BoolDatatype extends Datatype
case object IntDatatype extends Datatype
case class TupleDatatype(elements: List[Datatype]) extends Datatype
case class FunDatatype(params: List[Datatype], returnTypes: List[Datatype]) extends Datatype

case class Dataflow(value: Either[Fn, Op], idx: Int)
case class Controlflow(op: Op)

abstract class Op {

}

case class AddInt(addInts: List[Dataflow], subInts: List[Dataflow], next: Controlflow) extends Op
case class AndInt(ints: List[Dataflow], next: Controlflow) extends Op
case class OrInt(ints: List[Dataflow], next: Controlflow) extends Op
case class XorInt(ints: List[Dataflow], next: Controlflow) extends Op
case class MultInt(ints: List[Dataflow], next: Controlflow) extends Op
case class DivInt(intA: Dataflow, intB: Dataflow, next: Controlflow) extends Op
case class ModInt(intA: Dataflow, intB: Dataflow, next: Controlflow) extends Op
case class Branch(condition: Dataflow, ifTrue: Controlflow, ifFalse: Controlflow) extends Op
case class Phi(branch: Branch, ifTrue: Dataflow, ifFalse: Dataflow, next: Controlflow) extends Op
case class TupleIdx(tuple: Dataflow, idx: Int, next: Controlflow) extends Op
case class Ret(returnValues: List[Dataflow]) extends Op

case class Fn(entry: Controlflow, signature: FunDatatype)
