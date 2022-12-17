package opt

import core.roundUp

import scala.collection.mutable

abstract class Datatype {
  val (size, align) = this match {
    case UnitDatatype => (0, 1)
    case IntDatatype => (8, 8)
    case BoolDatatype => (8, 8)
    case RefDatatype(_) => (8, 8)
    case FunDatatype(_, _) => (8, 8)
    case TupleDatatype(elements) =>
      val (size, align, _) = tupleLayout(elements)
      (size, align)
  }
}

case object UnitDatatype extends Datatype
case object IntDatatype extends Datatype
case object BoolDatatype extends Datatype
case class RefDatatype(datatype: Datatype) extends Datatype
case class FunDatatype(params: List[Datatype], returnTypes: List[Datatype]) extends Datatype
case class TupleDatatype(elements: List[Datatype]) extends Datatype

def tupleLayout(datatypes: List[Datatype]): (Int, Int, List[Int]) = {
  var curSize = 0
  var maxAlign = 1
  val offsets = for (datatype <- datatypes) yield {
    val offset = curSize.roundUp(datatype.align)
    curSize = offset + datatype.size
    maxAlign = maxAlign.max(datatype.align)
    offset
  }

  (curSize.roundUp(maxAlign), maxAlign, offsets)
}

abstract class ConstVal {
  def toGraph: (Op, OpNext) = this match {
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

def toPair[T](e: T): (T, T) = (e, e)

def linkOps(next: Ctrl)(ops: List[OpNext]): Op = ops match {
  case op :: rest =>
    op.next = linkOps(next)(rest)
    op
  case _ => next
}

def linkOpSections(next: Ctrl)(ops: List[(Op, OpNext)]): Op = ops match {
  case (firstOp, lastOp) :: rest =>
    lastOp.next = linkOpSections(next)(rest)
    firstOp
  case _ => next
}

def graph(lines: List[String]): String = s"${lines.map(" " * 8 + _).mkString("\n")}"

def graphNode(main: Op)(label: String): String = s"Op_${main.id} [label = \"$label\"]"

def graphDataEdge(main: Op, funIndex: Int)(color: String)(label: String)(data: Data): String = data match {
  case Data(Some(op), idx) => s"Op_${op.id} -> Op_${main.id} [color = $color, label = \"${Some(idx).filter(_ > 0).map(i => s"[$i] ").getOrElse("")}$label\"]" //
  case Data(None, idx) => s"Par_${funIndex}_$idx -> Op_${main.id} [color = $color, label = \"$label\"]"
}

def graphCtrlEdge(main: Op)(color: String)(ctrl: Ctrl): String = s"Op_${main.id} -> Op_${ctrl.id} [color = $color]"

enum BitwiseOp(val string: String) {
  case And extends BitwiseOp("&")
  case Or extends BitwiseOp("|")
  case Xor extends BitwiseOp("^")
}

enum CompType(val string: String) {
  case Pos extends CompType(">0")
  case PosOrZero extends CompType(">=0")
  case Zero extends CompType("==0")
  case NotZero extends CompType("!=0")
}

trait HasNext {
  def next: Ctrl
  def next_=(op: Ctrl): Unit
}

case class Block(var next: Ctrl = null) extends HasNext

class AbstractAsmContext {
  val data: mutable.Map[Data, (Option[asm.Src], Datatype)] = mutable.Map.empty
  private var regCounter = 0
  private var localCounter = 0
  private var globalCounter = 0

  def reg: Int = {
    val r = regCounter
    regCounter += 1
    r
  }

  def local(size: Int, align: Int = 8): Int = {
    localCounter = localCounter.roundUp(align)
    val l = localCounter
    localCounter += size
    l
  }

  def global(size: Int, align: Int = 8): Int = {
    globalCounter = globalCounter.roundUp(align)
    val l = globalCounter
    globalCounter += size
    l
  }
}

abstract class Op {
  val id: Long = OptUnit.nextId

  def format(funIndex: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): List[String] = {
    val node = graphNode(this)
    val dataEdge = graphDataEdge(this, funIndex)("purple")("")
    val dataEdgeSec = graphDataEdge(this, funIndex)("blue")("")
    val dataEdgeLabel = graphDataEdge(this, funIndex)("purple")
    val ctrlEdge = graphCtrlEdge(this)("orange")
    val ctrlEdgeIf = graphCtrlEdge(this)("red")
    val ctrlEdgeElse = graphCtrlEdge(this)("green")

    def recur: List[String] = this match {
      case hasNext: HasNext => ctrlEdge(hasNext.next) :: hasNext.next.format(funIndex, funIds, varIds)
      case _ => List()
    }

    this match {
      case UnitLit() => node("unit") :: recur
      case IntLit(int) => node(int.toString) :: recur
      case BoolLit(bool) => node(bool.toString) :: recur
      case FunLit(fun) => node(s"function[${funIds(fun)}]") :: recur
      case TupleLit(elements) => node(s"tuple") :: (recur ::: elements.map(dataEdge))
      case AddInt(addInts, subInts) => node("+") :: (recur ::: addInts.map(dataEdge) ::: subInts.map(dataEdgeLabel("Negate")))
      case BitwiseInt(bitwiseOp, ints) => node(bitwiseOp.string) :: (recur ::: ints.map(dataEdge))
      case MultInt(ints) => node("*") :: (recur ::: ints.map(dataEdge))
      case DivInt(dividend, divisor) => node("/") :: dataEdgeLabel("Dividend")(dividend) :: dataEdgeLabel("Divisor")(divisor) :: recur
      case CompInt(compType, int) => node(compType.string) :: dataEdge(int) :: recur
      case If(condition, ifBlock, elseBlock) => node("if") :: dataEdge(condition) :: ctrlEdgeIf(ifBlock.next) :: ctrlEdgeElse(elseBlock.next) :: (ifBlock.next.format(funIndex, funIds, varIds) ::: elseBlock.next.format(funIndex, funIds, varIds) ::: recur)
      case EndIf(_, returnValues) => node("end if") :: returnValues.map(dataEdge)
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
      case PrintI64(data) => node("printi64") :: dataEdge(data) :: recur
      case Call(Left(fun), values) => node(s"invoke[${funIds(fun)}]") :: (recur ::: values.map(dataEdge))
      case Call(Right(fun), values) => node("invoke") :: dataEdgeSec(fun) :: (recur ::: values.map(dataEdge))
      case Ret(returnValues) => node("return") :: returnValues.map(dataEdge)
    }
  }

  def generateAbstractAsm(context: AbstractAsmContext) = this match
    case UnitLit() =>
      context.data(Data(this)) = (None, UnitDatatype)
    case IntLit(int) =>
      context.data(Data(this)) = (Some(asm.Imm(int)), IntDatatype)
    case BoolLit(bool) =>
      context.data(Data(this)) = (Some(asm.Imm(if bool then 1 else 0)), BoolDatatype)
    case FunLit(fun) =>
      context.data(Data(this)) = (Some(asm.Lab(s"F${fun.id}")), fun.signature)
    case TupleLit(elements) =>
      val tupleType = TupleDatatype(elements.map(e => context.data(e)._2))
      val local = context.local(tupleType.size, tupleType.align)

    case AddInt(addInts, subInts) => ???
    case BitwiseInt(bitwiseOp, ints) => ???
    case MultInt(ints) => ???
    case DivInt(dividend, divisor) => ???
    case CompInt(compType, int) => ???
    case If(condition, ifBlock, elseBlock) => ???
    case EndIf(_, returnValues) => ???
    case TupleIdx(tuple, idx) => ???
    case ReadRef(ref) => ???
    case WriteRef(ref, data) => ???
    case RefValue(data) => ???
    case ReadLocal(local) => ???
    case WriteLocal(local, data) => ???
    case RefLocal(local) => ???
    case ReadGlobal(global, idx) => ???
    case WriteGlobal(global, idx, data) => ???
    case RefGlobal(global, idx) => ???
    case PrintI64(data) => ???
    case Call(Left(fun), values) => ???
    case Call(Right(fun), values) => ???
    case Ret(returnValues) => ???
}

abstract class OpNext(var next: Ctrl = null) extends Op with HasNext

case class UnitLit() extends OpNext
case class IntLit(var int: Long) extends OpNext
case class BoolLit(var bool: Boolean) extends OpNext
case class FunLit(var fun: Fun) extends OpNext
case class TupleLit(var elements: List[Data]) extends OpNext
case class AddInt(var addInts: List[Data], var subInts: List[Data]) extends OpNext
case class BitwiseInt(var bitwiseOp: BitwiseOp, var ints: List[Data]) extends OpNext
case class MultInt(var ints: List[Data]) extends OpNext
case class DivInt(var dividend: Data, var divisor: Data) extends OpNext
case class CompInt(var compType: CompType, var int: Data) extends OpNext
case class If(var condition: Data, var ifBlock: Block, var elseBlock: Block) extends OpNext
case class EndIf(var ifNode: If, var returnValues: List[Data]) extends Op
case class TupleIdx(var tuple: Data, var idx: Int) extends OpNext
case class ReadRef(var ref: Data) extends OpNext
case class WriteRef(var ref: Data, var data: Data) extends OpNext
case class RefValue(var data: Data) extends OpNext
case class ReadLocal(var local: Int) extends OpNext
case class WriteLocal(var local: Int, var data: Data) extends OpNext
case class RefLocal(var local: Int) extends OpNext
case class ReadGlobal(var global: Var, var idx: Int) extends OpNext
case class WriteGlobal(var global: Var, var idx: Int, var data: Data) extends OpNext
case class RefGlobal(var global: Var, var idx: Int) extends OpNext
case class PrintI64(var data: Data) extends OpNext
case class Call(var fun: Either[Fun, Data], var values: List[Data]) extends OpNext
case class Ret(var returnValues: List[Data]) extends Op

class Fun(var next: Ctrl = null, var signature: FunDatatype = null, var localVars: List[Datatype] = null) extends HasNext {
  val id: Long = OptUnit.nextId

  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(s"Fun_$index [shape = box, label = \"function[$index]\"]" :: s"Fun_$index -> Op_${next.id} [color = orange]" :: Range(0, signature.params.length).toList.flatMap { paramIndex =>
    List(s"Par_${index}_$paramIndex [shape = diamond, label = \"parameter[$paramIndex]\"]", s"Fun_$index -> Par_${index}_$paramIndex [color = green]")
  } ::: next.format(index, funIds, varIds))
}

class Var(var next: Ctrl = null, var localVars: List[Datatype] = null) extends HasNext {
  val id: Long = OptUnit.nextId

  def format(index: Int, funIds: Map[Fun, Int], varIds: Map[Var, Int]): String = graph(s"Var_$index [shape = diamond, label = \"global[$index]\"]" :: s"Var_$index -> Op_${next.id} [color = orange]" :: next.format(0, funIds, varIds))
}

class OptUnit(var mainFun: Fun, var funs: List[Fun], var vars: List[Var]) {
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
