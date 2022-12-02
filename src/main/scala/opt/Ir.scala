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
  def toGraph(nextCtrl: Controlflow): (Dataflow, Controlflow) = this match {
    case ConstUnit =>
      lazy val unitOp: Op = UnitLit(nextCtrl)
      lazy val unitData: Dataflow = Dataflow(() => Some(unitOp))
      lazy val unitCtrl: Controlflow = Controlflow(() => unitOp)
      (unitData, unitCtrl)
    case ConstInt(int) =>
      lazy val intOp: Op = IntLit(int, nextCtrl)
      lazy val intData: Dataflow = Dataflow(() => Some(intOp))
      lazy val intCtrl: Controlflow = Controlflow(() => intOp)
      (intData, intCtrl)
    case ConstBool(bool) =>
      lazy val boolOp: Op = BoolLit(bool, nextCtrl)
      lazy val boolData: Dataflow = Dataflow(() => Some(boolOp))
      lazy val boolCtrl: Controlflow = Controlflow(() => boolOp)
      (boolData, boolCtrl)
    case ConstTuple(elements) =>
      lazy val (elementsCtrl: Controlflow, elementsData: List[Dataflow]) = elements.foldRight((tupleCtrl, List[Dataflow]()))((constVal, tuple) => {
        val (next, dataAcc) = tuple
        lazy val (constData: Dataflow, constCtrl: Controlflow) = constVal.toGraph(next)
        (constCtrl, constData :: dataAcc)
      })

      lazy val tupleOp: Op = TupleLit(elementsData, nextCtrl)
      lazy val tupleData: Dataflow = Dataflow(() => Some(tupleOp))
      lazy val tupleCtrl: Controlflow = Controlflow(() => tupleOp)
      (tupleData, elementsCtrl)
    case ConstFun(fun) =>
      lazy val funOp: Op = FunLit(fun, nextCtrl)
      lazy val funData: Dataflow = Dataflow(() => Some(funOp))
      lazy val funCtrl: Controlflow = Controlflow(() => funOp)
      (funData, funCtrl)
  }
}

case object ConstUnit extends ConstVal
case class ConstInt(int: Long) extends ConstVal
case class ConstBool(bool: Boolean) extends ConstVal
case class ConstTuple(elements: List[ConstVal]) extends ConstVal
case class ConstFun(fun: () => Fun) extends ConstVal

case class Dataflow(valueLazy: () => Option[Op], idx: Int = 0) {
  lazy val value: Option[Op] = valueLazy()
}

object Dataflow {
  def unapply(dataflow: Dataflow): Option[(Option[Op], Int)] = Some((dataflow.value, dataflow.idx))
}

case class Controlflow(opLazy: () => Op) {
  lazy val op: Op = opLazy()
}

object Controlflow {
  def unapply(controlflow: Controlflow): Option[Op] = Some(controlflow.op)
}

def graph(lines: List[String]): String = s"${lines.map(" " * 8 + _).mkString("\n")}"

def graph_node(main: Op)(label: String): String = s"Op_${main.id} [label = \"$label\"]"

def graph_data_edge(main: Op, funIndex: Int)(dataflow: Dataflow): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = purple]"
  case None => s"Par_${funIndex}_${dataflow.idx} -> Op_${main.id} [color = purple]"
}

def graph_data_edge_sec(main: Op, funIndex: Int)(dataflow: Dataflow): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = blue]"
  case None => s"Par_${funIndex}_${dataflow.idx} -> Op_${main.id} [color = blue]"
}

def graph_data_edge_label(main: Op, funIndex: Int)(label: String)(dataflow: Dataflow): String = dataflow.value match {
  case Some(op) => s"Op_${op.id} -> Op_${main.id} [color = purple, label = \"$label\"]"
  case None => s"Par_${funIndex}_${dataflow.idx} -> Op_${main.id} [color = purple, label = \"$label\"]"
}

def graph_ctrl_edge(main: Op)(controlflow: Controlflow): String = s"Op_${main.id} -> Op_${controlflow.op.id} [color = orange]"

def graph_ctrl_edge_sec(main: Op)(controlflow: Controlflow): String = s"Op_${main.id} -> Op_${controlflow.op.id} [color = red]"

def graph_phi_edge(main: Op)(branch: Branch): String = s"Op_${main.id} -> Op_${branch.id} [color = green]"

abstract class Op {
  lazy val id: Long = {
    Op.idCounter += 1
    Op.idCounter
  }

  def format(formatted: mutable.Set[Long], funIndex: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): List[String] = if (!formatted.contains(id)) {
    formatted.add(id)

    val node = graph_node(this)
    val data_edge = graph_data_edge(this, funIndex)
    val data_edge_sec = graph_data_edge_sec(this, funIndex)
    val data_edge_label = graph_data_edge_label(this, funIndex)
    val ctrl_edge = graph_ctrl_edge(this)
    val ctrl_edge_sec = graph_ctrl_edge_sec(this)
    val phi_edge = graph_phi_edge(this)

    def recur(next: Controlflow): List[String] = next.op.format(formatted, funIndex, funIds, varIds)

    this match {
      case UnitLit(next) => node("unit") :: ctrl_edge(next) :: recur(next)
      case IntLit(int, next) => node(int.toString) :: ctrl_edge(next) :: recur(next)
      case BoolLit(bool, next) => node(bool.toString) :: ctrl_edge(next) :: recur(next)
      case FunLit(fun, next) => node(s"function[${funIds(fun())}]") :: ctrl_edge(next) :: recur(next)
      case TupleLit(elements, next) => node(s"tuple") :: ctrl_edge(next) :: (recur(next) ::: elements.map(data_edge))
      case AddInt(addInts, subInts, next) => node("+") :: ctrl_edge(next) :: (recur(next) ::: addInts.map(data_edge) ::: subInts.map(data_edge_label("Negate")))
      case AndInt(ints, next) => node("&") :: ctrl_edge(next) :: (recur(next) ::: ints.map(data_edge))
      case OrInt(ints, next) => node("|") :: ctrl_edge(next) :: (recur(next) ::: ints.map(data_edge))
      case XorInt(ints, next) => node("^") :: ctrl_edge(next) :: (recur(next) ::: ints.map(data_edge))
      case MultInt(ints, next) => node("*") :: ctrl_edge(next) :: (recur(next) ::: ints.map(data_edge))
      case DivInt(dividend, divisor, next) => node("/") :: ctrl_edge(next) :: data_edge_label("Dividend")(dividend) :: data_edge_label("Divisor")(divisor) :: recur(next)
      case ModInt(dividend, divisor, next) => node("%") :: ctrl_edge(next) :: data_edge_label("Dividend")(dividend) :: data_edge_label("Divisor")(divisor) :: recur(next)
      case IsGreater(int, next) => node(">0") :: ctrl_edge(next) :: data_edge(int) :: recur(next)
      case IsGreaterOrZero(int, next) => node(">=0") :: ctrl_edge(next) :: data_edge(int) :: recur(next)
      case IsZero(int, next) => node("==0") :: ctrl_edge(next) :: data_edge(int) :: recur(next)
      case IsNotZero(int, next) => node("!=0") :: ctrl_edge(next) :: data_edge(int) :: recur(next)
      case Branch(condition, ifTrue, ifFalse) => node("branch") :: data_edge(condition) :: ctrl_edge(ifTrue) :: ctrl_edge_sec(ifFalse) :: (recur(ifTrue) ::: recur(ifFalse))
      case Phi(branch, ifTrue, ifFalse, next) => node("phi") :: phi_edge(branch) :: data_edge(ifTrue) :: data_edge_sec(ifFalse) :: recur(next)
      case TupleIdx(tuple, idx, next) => node(s"tuple[$idx]") :: data_edge(tuple) :: recur(next)
      case ReadRef(ref, next) => node(s"val") :: data_edge(ref) :: ctrl_edge(next) :: recur(next)
      case WriteRef(ref, data, next) => node(s"ref = ") :: data_edge(ref) :: data_edge_sec(data) :: ctrl_edge(next) :: recur(next)
      case ReadLocal(local, next) => node(s"local[$local]") :: ctrl_edge(next) :: recur(next)
      case WriteLocal(local, data, next) => node(s"local[$local] = ") :: data_edge(data) :: ctrl_edge(next) :: recur(next)
      case RefLocal(local, next) => node(s"ref local[$local]") :: ctrl_edge(next) :: recur(next)
      case ReadGlobal(global, idx, next) => node(s"global[${varIds(global())}][$idx]") :: ctrl_edge(next) :: recur(next)
      case WriteGlobal(global, idx, data, next) => node(s"global[${varIds(global())}][$idx] = ") :: data_edge(data) :: ctrl_edge(next) :: recur(next)
      case RefGlobal(global, idx, next) => node(s"ref global[${varIds(global())}][$idx]") :: ctrl_edge(next) :: recur(next)
      case Call(fun, values, next) => node(s"invoke[${funIds(fun())}]") :: ctrl_edge(next) :: (recur(next) ::: values.map(data_edge))
      case CallInd(fun, values, next) => node("invoke") :: ctrl_edge(next) :: data_edge_sec(fun) :: (recur(next) ::: values.map(data_edge))
      case Ret(returnValues) => node("return") :: returnValues.map(data_edge)
    }
  } else List.empty
}

object Op {
  private var idCounter = 0
}

case class UnitLit(next: Controlflow) extends Op
case class IntLit(int: Long, next: Controlflow) extends Op
case class BoolLit(bool: Boolean, next: Controlflow) extends Op
case class FunLit(fun: () => Fun, next: Controlflow) extends Op
case class TupleLit(elements: List[Dataflow], next: Controlflow) extends Op
case class AddInt(addInts: List[Dataflow], subInts: List[Dataflow], next: Controlflow) extends Op
case class AndInt(ints: List[Dataflow], next: Controlflow) extends Op
case class OrInt(ints: List[Dataflow], next: Controlflow) extends Op
case class XorInt(ints: List[Dataflow], next: Controlflow) extends Op
case class MultInt(ints: List[Dataflow], next: Controlflow) extends Op
case class DivInt(dividend: Dataflow, divisor: Dataflow, next: Controlflow) extends Op
case class ModInt(dividend: Dataflow, divisor: Dataflow, next: Controlflow) extends Op
case class IsGreater(int: Dataflow, next: Controlflow) extends Op
case class IsGreaterOrZero(int: Dataflow, next: Controlflow) extends Op
case class IsZero(int: Dataflow, next: Controlflow) extends Op
case class IsNotZero(int: Dataflow, next: Controlflow) extends Op
case class Branch(condition: Dataflow, ifTrue: Controlflow, ifFalse: Controlflow) extends Op
case class Phi(branch: Branch, ifTrue: Dataflow, ifFalse: Dataflow, next: Controlflow) extends Op
case class TupleIdx(tuple: Dataflow, idx: Int, next: Controlflow) extends Op
case class ReadRef(ref: Dataflow, next: Controlflow) extends Op
case class WriteRef(ref: Dataflow, data: Dataflow, next: Controlflow) extends Op
case class ReadLocal(local: Int, next: Controlflow) extends Op
case class WriteLocal(local: Int, data: Dataflow, next: Controlflow) extends Op
case class RefLocal(local: Int, next: Controlflow) extends Op
case class ReadGlobal(global: () => Var, idx: Int, next: Controlflow) extends Op
case class WriteGlobal(global: () => Var, idx: Int, data: Dataflow, next: Controlflow) extends Op
case class RefGlobal(global: () => Var, idx: Int, next: Controlflow) extends Op
case class Call(fun: () => Fun, values: List[Dataflow], next: Controlflow) extends Op
case class CallInd(fun: Dataflow, values: List[Dataflow], next: Controlflow) extends Op
case class Ret(returnValues: List[Dataflow]) extends Op

abstract class Fun {
  def signature: FunDatatype

  def formatParams(funIndex: Int): List[String] = Range(0, signature.params.length).toList.flatMap { index =>
    List(s"Par_${funIndex}_$index [shape = diamond, label = \"parameter[$index]\"]", s"Fun_$funIndex -> Par_${funIndex}_$index [color = green]")
  }

  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String
}

class CodeFun(val entry: Controlflow, val signature: FunDatatype, val localVars: List[Datatype]) extends Fun {
  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(s"Fun_$index [shape = box, label = \"function[$index]\"]" :: s"Fun_$index -> Op_${entry.op.id} [color = orange]" :: formatParams(index) ::: entry.op.format(mutable.Set(), index, funIds, varIds))
}

class AsmFun(val instr: List[gen.Instr], val signature: FunDatatype) extends Fun {
  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(List(s"Fun_$index [shape = box, label = \"function[$index]\"]"))
}

class Var(val entry: Controlflow, val datatypes: Array[Datatype], val localVars: List[Datatype]) {
  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(s"Var_$index [shape = diamond, label = \"global[$index]\"]" :: s"Var_$index -> Op_${entry.op.id} [color = orange]" :: entry.op.format(mutable.Set(), 0, funIds, varIds))
}

case class BssElement(size: Int, align: Int)
case class ConstElement(strings: List[String], size: Int, align: Int)
case class DataElement(strings: List[String], size: Int, align: Int)

case class StaticData(bss: Map[String, BssElement], const: Map[String, ConstElement], data: Map[String, DataElement], windowsFunctions: List[String])

class OptUnit(val funs: List[Fun], val vars: List[Var], val staticData: StaticData) {
  def transformFuns(transformer: PartialFunction[(Op, Controlflow => Controlflow, Dataflow => Dataflow), Op]): List[Fun] = {
    funs.map { fun =>
      val transformed = mutable.Map[Op, Op]()

      def mapData(data: Dataflow): Dataflow = Dataflow(() => data.value.map(op => Some(transform(op))), data.idx)

      def mapCtrl(ctrl: Controlflow): Controlflow = Controlflow(() => transform(ctrl.op))

      def transform(op: Op): Op = transformed.getOrElse(op, transformer.applyOrElse((op, mapCtrl, mapData), {
        case UnitLit(next) => UnitLit(mapCtrl(next))
        case IntLit(int, next) => IntLit(int, mapCtrl(next))
        case BoolLit(bool, next) => BoolLit(bool, mapCtrl(next))
        case FunLit(fun, next) => FunLit(fun, mapCtrl(next))
        case TupleLit(elements, next) => TupleLit(elements.map(mapData), mapCtrl(next))
        case AddInt(addInts, subInts, next) => ???
        case AndInt(ints, next) => ???
        case OrInt(ints, next) => ???
        case XorInt(ints, next) => ???
        case MultInt(ints, next) => ???
        case DivInt(dividend, divisor, next) => ???
        case ModInt(dividend, divisor, next) => ???
        case IsGreater(int, next) => ???
        case IsGreaterOrZero(int, next) => ???
        case IsZero(int, next) => ???
        case IsNotZero(int, next) => ???
        case Branch(condition, ifTrue, ifFalse) => ???
        case Phi(branch, ifTrue, ifFalse, next) => ???
        case TupleIdx(tuple, idx, next) => ???
        case ReadRef(ref, next) => ???
        case WriteRef(ref, data, next) => ???
        case ReadLocal(local, next) => ???
        case WriteLocal(local, data, next) => ???
        case RefLocal(local, next) => ???
        case ReadGlobal(global, idx, next) => ???
        case WriteGlobal(global, idx, data, next) => ???
        case RefGlobal(global, idx, next) => ???
        case Call(fun, values, next) => ???
        case CallInd(fun, values, next) => ???
        case Ret(returnValues) => ???
      }))

      fun match {
        case codeFun: CodeFun => new CodeFun(mapCtrl(codeFun.entry), codeFun.signature, codeFun.localVars)
      }
    }
  }
  def format(): String = {
    val funIds: Map[Fun, Int] = funs.zipWithIndex.toMap
    val varIds: Map[Var, Int] = vars.zipWithIndex.toMap
    (funs.zipWithIndex.map { case (f, index) => f.format(index, funIds, varIds) } ::: vars.zipWithIndex.map { case (v, index) => v.format(index, funIds, varIds) }).mkString("digraph {\n", "\n", "\n}")
  }
}
