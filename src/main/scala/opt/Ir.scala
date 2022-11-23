package opt

import gen.AsmGen.label

import scala.collection.mutable

abstract class Datatype {

}

case object UnitDatatype extends Datatype
case object IntDatatype extends Datatype
case object BoolDatatype extends Datatype
case class TupleDatatype(elements: List[Datatype]) extends Datatype
case class FunDatatype(params: List[Datatype], returnTypes: List[Datatype]) extends Datatype

case class Dataflow(valueLazy: () => Option[Op], idx: Int = 0) {
  lazy val value = valueLazy()
}

object Dataflow {
  def unapply(dataflow: Dataflow): Option[(Option[Op], Int)] = Some((dataflow.value, dataflow.idx))
}

case class Controlflow(opLazy: () => Op) {
  lazy val op = opLazy()
}

object Controlflow {
  def unapply(controlflow: Controlflow): Option[Op] = Some(controlflow.op)
}

def graph_node(main: Op)(label: String): String = s"Op_${main.id} [label = \"$label\"]"

def graph_data_edge(main: Op)(dataflow: Dataflow): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = purple]"
  case None => s"Par_${dataflow.idx} -> Op_${main.id} [color = purple]"
}

def graph_data_edge_sec(main: Op)(dataflow: Dataflow): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = blue]"
  case None => s"Par_${dataflow.idx} -> Op_${main.id} [color = blue]"
}

def graph_data_edge_label(main: Op)(label: String)(dataflow: Dataflow): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = purple, label = \"$label\"]"
  case None => s"Par_${dataflow.idx} -> Op_${main.id} [color = purple, label = '\"$label\"]"
}

def graph_ctrl_edge(main: Op)(controlflow: Controlflow): String = s"Op_${main.id} -> Op_${controlflow.op.id} [color = orange]"

def graph_ctrl_edge_sec(main: Op)(controlflow: Controlflow): String = s"Op_${main.id} -> Op_${controlflow.op.id} [color = red]"

def graph_phi_edge(main: Op)(branch: Branch): String = s"Op_${main.id} -> Op_${branch.id} [color = green]"

abstract class Op {
  lazy val id = (math.random() * 1e15).toLong

  def format(formatted: mutable.Set[Long]): List[String] = if (!formatted.contains(id)) {
    formatted.add(id)

    val node = graph_node(this)
    val data_edge = graph_data_edge(this)
    val data_edge_sec = graph_data_edge_sec(this)
    val data_edge_label = graph_data_edge_label(this)
    val ctrl_edge = graph_ctrl_edge(this)
    val ctrl_edge_sec = graph_ctrl_edge_sec(this)
    val phi_edge = graph_phi_edge(this)

    this match {
      case UnitLit(next) => node("()") :: ctrl_edge(next) :: next.op.format(formatted)
      case IntLit(int, next) => node(int.toString) :: ctrl_edge(next) :: next.op.format(formatted)
      case BoolLit(bool, next) => node(bool.toString) :: ctrl_edge(next) :: next.op.format(formatted)
      case FunLit(fn, next) => ???
      case AddInt(addInts, subInts, next) => node("+") :: ctrl_edge(next) :: (next.op.format(formatted) ::: addInts.map(data_edge) ::: subInts.map(data_edge_label("Negate")))
      case AndInt(ints, next) => node("&") :: ctrl_edge(next) :: (next.op.format(formatted) ::: ints.map(data_edge))
      case OrInt(ints, next) => node("|") :: ctrl_edge(next) :: (next.op.format(formatted) ::: ints.map(data_edge))
      case XorInt(ints, next) => node("^") :: ctrl_edge(next) :: (next.op.format(formatted) ::: ints.map(data_edge))
      case MultInt(ints, next) => node("*") :: ctrl_edge(next) :: (next.op.format(formatted) ::: ints.map(data_edge))
      case DivInt(dividend, divisor, next) => node("/") :: ctrl_edge(next) :: data_edge_label("Dividend")(dividend) :: data_edge_label("Divisor")(divisor) :: next.op.format(formatted)
      case ModInt(dividend, divisor, next) => node("%") :: ctrl_edge(next) :: data_edge_label("Dividend")(dividend) :: data_edge_label("Divisor")(divisor) :: next.op.format(formatted)
      case Branch(condition, ifTrue, ifFalse) => node("branch") :: data_edge(condition) :: ctrl_edge(ifTrue) :: ctrl_edge_sec(ifFalse) :: (ifTrue.op.format(formatted) ::: ifFalse.op.format(formatted))
      case Phi(branch, ifTrue, ifFalse, next) => node("phi") :: phi_edge(branch) :: data_edge(ifTrue) :: data_edge_sec(ifFalse) :: next.op.format(formatted)
      case TupleIdx(tuple, idx, next) => node(s"Tuple[$idx]") :: data_edge(tuple) :: next.op.format(formatted)
      case ReadRef(ref, next) => ???
      case WriteRef(ref, data, next) => ???
      case ReadLocal(local, next) => ???
      case WriteLocal(local, data, next) => ???
      case RefLocal(local, next) => ???
      case ReadGlobal(global, idx, next) => node(s"global $global[$idx]") :: ctrl_edge(next) :: next.op.format(formatted)
      case WriteGlobal(global, idx, data, next) => ???
      case RefGlobal(global, idx, next) => ???
      case Call(fn, values, next) => ???
      case CallInd(fn, values, next) => node("invoke") :: ctrl_edge(next) :: data_edge_sec(fn) :: (next.op.format(formatted) ::: values.map(data_edge))
      case Ret(returnValues) => node("return") :: returnValues.map(data_edge)
    }
  } else List.empty
}

case class UnitLit(next: Controlflow) extends Op
case class IntLit(int: Long, next: Controlflow) extends Op
case class BoolLit(bool: Boolean, next: Controlflow) extends Op
case class FunLit(fn: Int, next: Controlflow) extends Op
case class AddInt(addInts: List[Dataflow], subInts: List[Dataflow], next: Controlflow) extends Op
case class AndInt(ints: List[Dataflow], next: Controlflow) extends Op
case class OrInt(ints: List[Dataflow], next: Controlflow) extends Op
case class XorInt(ints: List[Dataflow], next: Controlflow) extends Op
case class MultInt(ints: List[Dataflow], next: Controlflow) extends Op
case class DivInt(dividend: Dataflow, divisor: Dataflow, next: Controlflow) extends Op
case class ModInt(dividend: Dataflow, divisor: Dataflow, next: Controlflow) extends Op
case class Branch(condition: Dataflow, ifTrue: Controlflow, ifFalse: Controlflow) extends Op
case class Phi(branch: Branch, ifTrue: Dataflow, ifFalse: Dataflow, next: Controlflow) extends Op
case class TupleIdx(tuple: Dataflow, idx: Int, next: Controlflow) extends Op
case class ReadRef(ref: Dataflow, next: Controlflow) extends Op
case class WriteRef(ref: Dataflow, data: Dataflow, next: Controlflow) extends Op
case class ReadLocal(local: Int, next: Controlflow) extends Op
case class WriteLocal(local: Int, data: Dataflow, next: Controlflow) extends Op
case class RefLocal(local: Int, next: Controlflow) extends Op
case class ReadGlobal(global: Int, idx: Int, next: Controlflow) extends Op
case class WriteGlobal(global: Int, idx: Int, data: Dataflow, next: Controlflow) extends Op
case class RefGlobal(global: Int, idx: Int, next: Controlflow) extends Op
case class Call(fn: Int, values: List[Dataflow], next: Controlflow) extends Op
case class CallInd(fn: Dataflow, values: List[Dataflow], next: Controlflow) extends Op
case class Ret(returnValues: List[Dataflow]) extends Op

abstract class Fun {

}

case class CodeFun(entry: Controlflow, signature: FunDatatype, localVars: List[Datatype]) extends Fun {
  def format(): String = s"digraph {${entry.op.format(mutable.Set()).map(" " * 8 + _).mkString("\n", "\n", "\n")}}"
}

case class AsmFun() extends Fun {

}

case class WindowsFun(name: String) extends Fun {

}

case class Var(entry: Controlflow, datatypes: Array[Datatype])

case class OptUnit(fns: Array[Fun], vars: Array[Var])

def fn: Fun = {
  lazy val int1: IntLit = IntLit(3, int2ctrl)
  lazy val int2: IntLit = IntLit(5, addctrl)
  lazy val add: AddInt = AddInt(List(int1data, int2data), List.empty, int3ctrl)
  lazy val int3: IntLit = IntLit(8, multctrl)
  lazy val mult: MultInt = MultInt(List(int3data, adddata), retctrl)
  lazy val ret: Ret = Ret(List(multdata))

  lazy val int1ctrl: Controlflow = Controlflow(() => int1)
  lazy val int2ctrl: Controlflow = Controlflow(() => int2)
  lazy val addctrl: Controlflow = Controlflow(() => add)
  lazy val int3ctrl: Controlflow = Controlflow(() => int3)
  lazy val multctrl: Controlflow = Controlflow(() => mult)
  lazy val retctrl: Controlflow = Controlflow(() => ret)

  lazy val int1data: Dataflow = Dataflow(() => Some(int1))
  lazy val int2data: Dataflow = Dataflow(() => Some(int2))
  lazy val adddata: Dataflow = Dataflow(() => Some(add))
  lazy val int3data: Dataflow = Dataflow(() => Some(int3))
  lazy val multdata: Dataflow = Dataflow(() => Some(mult))

  CodeFun(int1ctrl, FunDatatype(List.empty, List.empty), List.empty)
}
