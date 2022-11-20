package opt

import gen.AsmGen.label

import scala.collection.mutable

abstract class Datatype {

}

case object BoolDatatype extends Datatype
case object IntDatatype extends Datatype
case class TupleDatatype(elements: List[Datatype]) extends Datatype
case class FunDatatype(params: List[Datatype], returnTypes: List[Datatype]) extends Datatype

case class Dataflow(valueLazy: () => Option[Op], idx: Int = 0) {
  lazy val value = valueLazy()
}

case class Controlflow(opLazy: () => Op) {
  lazy val op = opLazy()
}

def graph_node(main: Op)(label: String): String = s"Op_${main.id} [label = \"$label\"]"

def graph_data_edge(main: Op)(dataflow: Dataflow): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = purple]"
  case None => s"Par_${dataflow.idx} -> Op_${main.id} [color = purple]"
}

def graph_data_edge_label(main: Op)(dataflow: Dataflow, label: String): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = purple, label = \"$label\"]"
  case None => s"Par_${dataflow.idx} -> Op_${main.id} [color = purple, label = '\"$label\"]"
}

def graph_ctrl_edge(main: Op)(controlflow: Controlflow): String = s"Op_${main.id} -> Op_${controlflow.op.id} [color = orange]"

abstract class Op {
  lazy val id = (math.random() * 1e15).toLong

  def format(formatted: mutable.Set[Long]): List[String] = if (!formatted.contains(id)) {
    formatted.add(id)

    val node = graph_node(this)
    val data_edge = graph_data_edge(this)
    val data_edge_label = graph_data_edge_label(this)
    val ctrl_edge = graph_ctrl_edge(this)

    this match {
      case IntLit(int, next) => node(int.toString) :: ctrl_edge(next) :: next.op.format(formatted)
      case BoolLit(bool, next) => node(bool.toString) :: ctrl_edge(next) :: next.op.format(formatted)
      case AddInt(addInts, subInts, next) => node("+") :: ctrl_edge(next) :: (next.op.format(formatted) ::: addInts.map(data_edge) ::: subInts.map(data_edge))
      case Ret(returnValues) => node("return") :: returnValues.map(data_edge)
    }
  } else List()
}

case class IntLit(int: Int, next: Controlflow) extends Op
case class BoolLit(bool: Boolean, next: Controlflow) extends Op
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
case class Call(fn: Fn, values: List[Dataflow], next: Controlflow) extends Op
case class CallInd(fn: Dataflow, values: List[Dataflow], next: Controlflow) extends Op
case class Ret(returnValues: List[Dataflow]) extends Op

case class Fn(entry: Controlflow, signature: FunDatatype) {
  def format(): String = s"digraph {\n${entry.op.format(mutable.Set()).map(" " * 8 + _ + "\n").mkString}}"
}

case class Var(entry: Controlflow, datatypes: List[Datatype])

def fn: Fn = {
  lazy val int1: IntLit = IntLit(3, int2ctrl)
  lazy val int2: IntLit = IntLit(5, addctrl)
  lazy val add: AddInt = AddInt(List(int1data, int2data), List(), retctrl)
  lazy val ret: Ret = Ret(List(adddata))

  lazy val int1ctrl: Controlflow = Controlflow(() => int1)
  lazy val int2ctrl: Controlflow = Controlflow(() => int2)
  lazy val addctrl: Controlflow = Controlflow(() => add)
  lazy val retctrl: Controlflow = Controlflow(() => ret)

  lazy val int1data: Dataflow = Dataflow(() => Some(int1))
  lazy val int2data: Dataflow = Dataflow(() => Some(int2))
  lazy val adddata: Dataflow = Dataflow(() => Some(add))

  Fn(int1ctrl, FunDatatype(List(), List()))
}
