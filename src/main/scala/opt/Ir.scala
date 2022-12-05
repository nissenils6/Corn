package opt

import gen.AsmGen.label
import sem.IrGenContext

import scala.collection.mutable

abstract class Datatype {

}

case object UnitDatatype extends Datatype
case object IntDatatype extends Datatype
case object BoolDatatype extends Datatype
case class RefDatatype(datatype: Datatype) extends Datatype
case class TupleDatatype(elements: List[Datatype]) extends Datatype
case class FunDatatype(params: List[Datatype], returnTypes: List[Datatype]) extends Datatype

abstract class ConstVal {
  def toGraph: (Op, Op) = this match {
    case ConstUnit => toPair(UnitLit())
    case ConstInt(int) => toPair(IntLit(int))
    case ConstBool(bool) => toPair(BoolLit(bool))
    case ConstTuple(elements) =>
      val elementOps = elements.map(_.toGraph)
      val tupleOp = TupleLit(elementOps.map(_._2).map(Data.apply))
      val firstElementOp = linkOpSections(tupleOp)(elementOps)
      (firstElementOp, tupleOp)
    case ConstFun(fun) => toPair(FunLit(fun))
  }
}

case object ConstUnit extends ConstVal
case class ConstInt(int: Long) extends ConstVal
case class ConstBool(bool: Boolean) extends ConstVal
case class ConstTuple(elements: List[ConstVal]) extends ConstVal
case class ConstFun(fun: Fun) extends ConstVal

type Ctrl = Op

case class Data(op: Option[Op], idx: Int) {
  override def equals(obj: Any): Boolean = (this, obj) match {
    case (Data(Some(op0), idx0), Data(Some(op1), idx1)) => (op0 eq op1) && (idx0 == idx1)
    case (Data(None, idx0), Data(None, idx1)) => idx0 == idx1
    case _ => false
  }
}

object Data {
  def apply(op: Op): Data = Data(Some(op), 0)
}

def toPair(op: Op): (Op, Op) = (op, op)

def linkOps(next: Ctrl)(ops: List[Op]): Op = ops match {
  case op :: rest =>
    op.next = linkOps(next)(rest)
    op
  case _ => next
}

def linkOpSections(next: Ctrl)(ops: List[(Op, Op)]): Op = ops match {
  case (firstOp, lastOp) :: rest =>
    lastOp.next = linkOpSections(next)(rest)
    firstOp
  case _ => next
}

def graph(lines: List[String]): String = s"${lines.map(" " * 8 + _).mkString("\n")}"

def graphNode(main: Op)(label: String): String = s"Op_${main.id} [label = \"$label\"]"

def graphDataEdge(main: Op, funIndex: Int)(data: Data): String = data match {
  case Data(Some(op), _) => s"Op_${op.id} -> Op_${main.id} [color = purple]"
  case Data(None, idx) => s"Par_${funIndex}_$idx -> Op_${main.id} [color = purple]"
}

def graphDataEdgeSec(main: Op, funIndex: Int)(data: Data): String = data match {
  case Data(Some(op), _) => s"Op_${op.id} -> Op_${main.id} [color = blue]"
  case Data(None, idx) => s"Par_${funIndex}_$idx -> Op_${main.id} [color = blue]"
}

def graphDataEdgeLabel(main: Op, funIndex: Int)(label: String)(data: Data): String = data match {
  case Data(Some(op), _) => s"Op_${op.id} -> Op_${main.id} [color = purple, label = \"$label\"]"
  case Data(None, idx) => s"Par_${funIndex}_$idx -> Op_${main.id} [color = purple, label = \"$label\"]"
}

def graphCtrlEdge(main: Op)(ctrl: Ctrl): String = s"Op_${main.id} -> Op_${ctrl.id} [color = orange]"

def graphCtrlEdgeSec(main: Op)(ctrl: Ctrl): String = s"Op_${main.id} -> Op_${ctrl.id} [color = red]"

def graphPhiEdge(main: Op)(branch: Branch): String = s"Op_${main.id} -> Op_${branch.id} [color = green]"

abstract class Op {
  val id = OptUnit.nextId

  var next: Ctrl = null

  def format(formatted: mutable.Set[Long], funIndex: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): List[String] = if (!formatted.contains(id)) {
    formatted.add(id)

    val node = graphNode(this)
    val dataEdge = graphDataEdge(this, funIndex)
    val dataEdgeSec = graphDataEdgeSec(this, funIndex)
    val dataEdgeLabel = graphDataEdgeLabel(this, funIndex)
    val ctrlEdge = graphCtrlEdge(this)
    val ctrlEdgeSec = graphCtrlEdgeSec(this)
    val phiEdge = graphPhiEdge(this)

    def recur: List[String] = ctrlEdge(next) :: next.format(formatted, funIndex, funIds, varIds)

    this match {
      case UnitLit() => node("unit") :: recur
      case IntLit(int) => node(int.toString) :: recur
      case BoolLit(bool) => node(bool.toString) :: recur
      case FunLit(fun) => node(s"function[${funIds(fun)}]") :: recur
      case TupleLit(elements) => node(s"tuple") :: (recur ::: elements.map(dataEdge))
      case AddInt(addInts, subInts) => node("+") :: (recur ::: addInts.map(dataEdge) ::: subInts.map(dataEdgeLabel("Negate")))
      case AndInt(ints) => node("&") :: (recur ::: ints.map(dataEdge))
      case OrInt(ints) => node("|") :: (recur ::: ints.map(dataEdge))
      case XorInt(ints) => node("^") :: (recur ::: ints.map(dataEdge))
      case MultInt(ints) => node("*") :: (recur ::: ints.map(dataEdge))
      case DivInt(dividend, divisor) => node("/") :: dataEdgeLabel("Dividend")(dividend) :: dataEdgeLabel("Divisor")(divisor) :: recur
      case ModInt(dividend, divisor) => node("%") :: dataEdgeLabel("Dividend")(dividend) :: dataEdgeLabel("Divisor")(divisor) :: recur
      case IsGreater(int) => node(">0") :: dataEdge(int) :: recur
      case IsGreaterOrZero(int) => node(">=0") :: dataEdge(int) :: recur
      case IsZero(int) => node("==0") :: dataEdge(int) :: recur
      case IsNotZero(int) => node("!=0") :: dataEdge(int) :: recur
      case Branch(condition, elseNext) => node("branch") :: dataEdge(condition) :: ctrlEdgeSec(elseNext) :: (recur ::: elseNext.format(formatted, funIndex, funIds, varIds))
      case Phi(branch, ifTrue, ifFalse) => node("phi") :: phiEdge(branch) :: dataEdge(ifTrue) :: dataEdgeSec(ifFalse) :: recur
      case TupleIdx(tuple, idx) => node(s"tuple[$idx]") :: dataEdge(tuple) :: recur
      case ReadRef(ref) => node(s"val") :: dataEdge(ref) :: recur
      case WriteRef(ref, data) => node(s"ref = ") :: dataEdge(ref) :: dataEdgeSec(data) :: recur
      case RefValue(data) => node(s"ref") :: dataEdge(data) :: recur
      case ReadLocal(local) => node(s"local[$local]") :: recur
      case WriteLocal(local, data) => node(s"local[$local] = ") :: dataEdge(data) :: recur
      case RefLocal(local) => node(s"ref local[$local]") :: recur
      case ReadGlobal(global, idx) => node(s"global[${varIds(global)}][$idx]") :: recur
      case WriteGlobal(global, idx, data) => node(s"global[${varIds(global)}][$idx] = ") :: dataEdge(data) :: recur
      case RefGlobal(global, idx) => node(s"ref global[${varIds(global)}][$idx]") :: recur
      case Call(Left(fun), values) => node(s"invoke[${funIds(fun)}]") :: (recur ::: values.map(dataEdge))
      case Call(Right(fun), values) => node("invoke") :: dataEdgeSec(fun) :: (recur ::: values.map(dataEdge))
      case Entry() => node("entry") :: recur
      case Ret(returnValues) => node("return") :: returnValues.map(dataEdge)
    }
  } else List.empty
}

case class UnitLit() extends Op
case class IntLit(var int: Long) extends Op
case class BoolLit(var bool: Boolean) extends Op
case class FunLit(var fun: Fun) extends Op
case class TupleLit(var elements: List[Data]) extends Op
case class AddInt(var addInts: List[Data], var subInts: List[Data]) extends Op
case class AndInt(var ints: List[Data]) extends Op
case class OrInt(var ints: List[Data]) extends Op
case class XorInt(var ints: List[Data]) extends Op
case class MultInt(var ints: List[Data]) extends Op
case class DivInt(var dividend: Data, var divisor: Data) extends Op
case class ModInt(var dividend: Data, var divisor: Data) extends Op
case class IsGreater(var int: Data) extends Op
case class IsGreaterOrZero(var int: Data) extends Op
case class IsZero(var int: Data) extends Op
case class IsNotZero(var int: Data) extends Op
case class Branch(var condition: Data, var elseNext: Ctrl = null) extends Op
case class Phi(var branch: Branch, var ifTrue: Data, var ifFalse: Data) extends Op
case class TupleIdx(var tuple: Data, var idx: Int) extends Op
case class ReadRef(var ref: Data) extends Op
case class WriteRef(var ref: Data, var data: Data) extends Op
case class RefValue(var data: Data) extends Op
case class ReadLocal(var local: Int) extends Op
case class WriteLocal(var local: Int, var data: Data) extends Op
case class RefLocal(var local: Int) extends Op
case class ReadGlobal(var global: Var, var idx: Int) extends Op
case class WriteGlobal(var global: Var, var idx: Int, var data: Data) extends Op
case class RefGlobal(var global: Var, var idx: Int) extends Op
case class Call(var fun: Either[Fun, Data], var values: List[Data]) extends Op
case class Entry() extends Op
case class Ret(var returnValues: List[Data]) extends Op

abstract class Fun {
  val id: Long = OptUnit.nextId

  def signature: FunDatatype

  def formatParams(funIndex: Int): List[String] = Range(0, signature.params.length).toList.flatMap { index =>
    List(s"Par_${funIndex}_$index [shape = diamond, label = \"parameter[$index]\"]", s"Fun_$funIndex -> Par_${funIndex}_$index [color = green]")
  }

  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String
}

class CodeFun(var entry: Entry = null, var signature: FunDatatype = null, var localVars: List[Datatype] = null) extends Fun {
  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(s"Fun_$index [shape = box, label = \"function[$index]\"]" :: s"Fun_$index -> Op_${entry.id} [color = orange]" :: formatParams(index) ::: entry.format(mutable.Set(), index, funIds, varIds))
}

class AsmFun(val instr: List[gen.Instr], val signature: FunDatatype) extends Fun {
  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(List(s"Fun_$index [shape = box, label = \"asm function[$index]\"]"))
}

class Var(var entry: Entry = null, var localVars: List[Datatype] = null) {
  val id: Long = OptUnit.nextId

  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(s"Var_$index [shape = diamond, label = \"global[$index]\"]" :: s"Var_$index -> Op_${entry.id} [color = orange]" :: entry.format(mutable.Set(), 0, funIds, varIds))
}

case class BssElement(size: Int, align: Int)
case class ConstElement(strings: List[String], size: Int, align: Int)
case class DataElement(strings: List[String], size: Int, align: Int)

case class StaticData(bss: Map[String, BssElement], const: Map[String, ConstElement], data: Map[String, DataElement], windowsFunctions: List[String])

class OptUnit(var funs: List[Fun], var vars: List[Var], val staticData: StaticData) {
  def format(): String = {
    val funIds: Map[Fun, Int] = funs.zipWithIndex.toMap
    val varIds: Map[Var, Int] = vars.zipWithIndex.toMap
    (funs.zipWithIndex.map { case (f, index) => f.format(index, funIds, varIds) } ::: vars.zipWithIndex.map { case (v, index) => v.format(index, funIds, varIds) }).mkString("digraph {\n", "\n", "\n}")
  }
}

object OptUnit {
  var ids: Long = 0

  def nextId: Long = {
    ids += 1
    ids
  }
}
