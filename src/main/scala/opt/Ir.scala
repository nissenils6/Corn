package opt

import asm.Src
import core.roundUp
import sem.ConstUnit.datatype
import sem.UnitDatatype

import java.time.temporal.TemporalQueries.offset
import scala.annotation.tailrec
import scala.collection.mutable

abstract class Datatype {
  lazy val (size, align) = this match {
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

def generateCopyAsm(context: AbstractAsmContext, datatype: Datatype, dst: asm.Dst, src: asm.Src): Unit = datatype match {
  case UnitDatatype => ()
  case IntDatatype | RefDatatype(_) | FunDatatype(_, _) =>
    context.instrs.append(asm.Mov(asm.DataSize.QWord, dst, src))
  case BoolDatatype =>
    context.instrs.append(asm.Mov(asm.DataSize.Byte, dst, src))
  case TupleDatatype(elements) => (dst, src) match {
    case (dstOff: asm.DstOff, srcOff: asm.DstOff) =>
      val offsets = tupleLayout(elements)._3
      elements.zip(offsets).foreach { case (subDatatype, offset) =>
        generateCopyAsm(context, subDatatype, dstOff.offset(offset), srcOff.offset(offset))
      }
  }
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

  override def hashCode(): Int = this match {
    case Data(Some(op), idx) => (op.id * 37 + 2711 + idx * 15331).toInt ^ 1781369
    case Data(None, idx) => (10247 + idx * 28439) ^ 531023
  }

  override def toString: String = this match {
    case Data(Some(op), idx) => s"Data(Some(${op.id}), $idx)"
    case Data(None, idx) => s"Data(None, $idx)"
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

enum CompType(val string: String, val condType: asm.CondType) {
  case Pos extends CompType(">0", asm.CondType.Gt)
  case PosOrZero extends CompType(">=0", asm.CondType.Gte)
  case Zero extends CompType("==0", asm.CondType.Eq)
  case NotZero extends CompType("!=0", asm.CondType.Neq)
}

trait HasNext {
  def next: Ctrl
  def next_=(op: Ctrl): Unit
}

case class Block(var next: Ctrl = null) extends HasNext

class AbstractAsmContext(optUnit: OptUnit) {
  val data: mutable.Map[Data, (Option[asm.Src], Datatype)] = mutable.Map.empty

  val funs: Map[Fun, Int] = optUnit.funs.zipWithIndex.toMap
  val globalVars: Map[Var, Array[Int]] = optUnit.vars.map(v => (v, v.localVars.map(d => global(d.size, d.align)).toArray)).toMap

  var instrs: mutable.Buffer[asm.Op] = mutable.Buffer.empty
  var localVars: Array[Int] = null
  var localTypes: Array[Datatype] = null
  var outputDsts: Array[Option[asm.Dst]] = null

  var regCount = 0
  var paramCount = 0
  var localCount = 0
  var globalCount = 0

  def src(d: Data): Src = data(d)._1.get
  def srcOption(d: Data): Option[Src] = data(d)._1
  def datatype(d: Data): Datatype = data(d)._2

  def reg(): Int = {
    val r = regCount
    regCount += 1
    r
  }

  def param(): Int = {
    val p = paramCount
    paramCount += 1
    p
  }

  def local(size: Int, align: Int = 8): Int = {
    localCount = localCount.roundUp(align)
    val l = localCount
    localCount += size
    l
  }

  def global(size: Int, align: Int = 8): Int = {
    globalCount = globalCount.roundUp(align)
    val l = globalCount
    globalCount += size
    l
  }

  def fun[T](locals: List[Datatype])(expression: => T): (Array[asm.Op], T) = {
    regCount = 0
    paramCount = 0
    localCount = 0
    globalCount = 0
    localVars = locals.map(d => local(d.size, d.align)).toArray
    localTypes = locals.toArray
    val value = expression
    val newInstrs = instrs.toArray
    instrs.clear()
    (newInstrs, value)
  }

  def block(newDsts: Array[Option[asm.Dst]])(expression: => Unit): Unit = {
    val savedDsts = outputDsts
    outputDsts = newDsts
    expression
    outputDsts = savedDsts
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
      case If(condition, ifBlock, elseBlock, _) => node("if") :: dataEdge(condition) :: ctrlEdgeIf(ifBlock.next) :: ctrlEdgeElse(elseBlock.next) :: (ifBlock.next.format(funIndex, funIds, varIds) ::: elseBlock.next.format(funIndex, funIds, varIds) ::: recur)
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
      case Print(data) => node("printi64") :: dataEdge(data) :: recur
      case Call(Left(fun), values) => node(s"invoke[${funIds(fun)}]") :: (recur ::: values.map(dataEdge))
      case Call(Right(fun), values) => node("invoke") :: dataEdgeSec(fun) :: (recur ::: values.map(dataEdge))
      case Ret(returnValues) => node("return") :: returnValues.map(dataEdge)
    }
  }

  final def generateAsm(context: AbstractAsmContext): Unit = {
    this match {
      case UnitLit() =>
        context.data(Data(this)) = (None, UnitDatatype)
      case IntLit(int) =>
        context.data(Data(this)) = (Some(asm.Imm(int)), IntDatatype)
      case BoolLit(bool) =>
        context.data(Data(this)) = (Some(asm.Imm(if bool then 1 else 0)), BoolDatatype)
      case FunLit(fun) =>
        val result = asm.Reg(context.reg())
        context.data(Data(this)) = (Some(result), fun.signature)
        context.instrs.append(asm.LeaFun(result, context.funs(fun)))
      case TupleLit(elements) =>
        val tupleType = TupleDatatype(elements.map(data => context.data(data)._2))
        val local = context.local(tupleType.size, tupleType.align)
        elements.map(context.data.apply).zip(tupleLayout(tupleType.elements)._3).foreach {
          case ((Some(src), datatype: Datatype), offset: Int) => generateCopyAsm(context, datatype, asm.Loc(local + offset), src)
          case _ => ()
        }
        context.data(Data(this)) = (Some(asm.Loc(local)), tupleType)
      case AddInt(List(a, b), List()) =>
        val result = asm.Reg(context.reg())
        context.data(Data(this)) = (Some(result), IntDatatype)
        context.instrs.append(asm.Add(result, context.src(a), context.src(b)))
      case AddInt(List(a), List(b)) =>
        val result = asm.Reg(context.reg())
        context.data(Data(this)) = (Some(result), IntDatatype)
        context.instrs.append(asm.Sub(result, context.src(a), context.src(b)))
      case AddInt(addInts, subInts) => ???
      case BitwiseInt(BitwiseOp.And, List(a, b)) =>
        val result = asm.Reg(context.reg())
        context.data(Data(this)) = (Some(result), IntDatatype)
        context.instrs.append(asm.And(result, context.src(a), context.src(b)))
      case BitwiseInt(BitwiseOp.Or, List(a, b)) =>
        val result = asm.Reg(context.reg())
        context.data(Data(this)) = (Some(result), IntDatatype)
        context.instrs.append(asm.Or(result, context.src(a), context.src(b)))
      case BitwiseInt(BitwiseOp.Xor, List(a, b)) =>
        val result = asm.Reg(context.reg())
        context.data(Data(this)) = (Some(result), IntDatatype)
        context.instrs.append(asm.Xor(result, context.src(a), context.src(b)))
      case BitwiseInt(bitwiseOp, ints) => ???
      case MultInt(List(a, b)) =>
        val result = asm.Reg(context.reg())
        context.data(Data(this)) = (Some(result), IntDatatype)
        context.instrs.append(asm.Mult(result, context.src(a), context.src(b)))
      case MultInt(ints) => ???
      case DivInt(dividend, divisor) =>
        val quotient = asm.Reg(context.reg())
        val remainder = asm.Reg(context.reg())
        context.data(Data(Some(this), 0)) = (Some(quotient), IntDatatype)
        context.data(Data(Some(this), 1)) = (Some(remainder), IntDatatype)
        context.instrs.append(asm.Div(Some(quotient), Some(remainder), context.src(dividend), context.src(divisor)))
      case CompInt(compType, Data(Some(AddInt(List(a), List(b))), 0)) =>
        val result = asm.Reg(context.reg())
        context.data(Data(Some(this), 0)) = (Some(result), BoolDatatype)
        context.instrs.append(asm.CSet(asm.Condition(compType.condType, context.src(a), context.src(b)), result))
      case CompInt(compType, int) =>
        val result = asm.Reg(context.reg())
        context.data(Data(Some(this), 0)) = (Some(result), BoolDatatype)
        context.instrs.append(asm.CSet(asm.Condition(compType.condType, context.src(int), asm.Imm(0)), result))
      case If(data, ifBlock, elseBlock, datatypes) =>
        val dsts = datatypes.map {
          case UnitDatatype => None: Option[asm.Dst]
          case tupleDatatype: TupleDatatype => Some(asm.Loc(context.local(tupleDatatype.size, tupleDatatype.align)))
          case _ => Some(asm.Reg(context.reg()))
        }.toArray

        val ifId = context.instrs.length
        val ifInstr = data match {
          case Data(Some(CompInt(compType, Data(Some(AddInt(List(a), List(b))), 0))), 0) => asm.If(asm.Condition(compType.condType, context.src(a), context.src(b)), 0)
          case _ => asm.If(asm.Condition(asm.CondType.Neq, context.src(data), asm.Imm(0)), 0)
        }
        context.instrs.append(ifInstr)
        context.block(dsts)(ifBlock.next.generateAsm(context))
        val elseId = context.instrs.length
        val elseInstr = asm.Else(ifId, 0)
        context.instrs.append(elseInstr)
        context.block(dsts)(elseBlock.next.generateAsm(context))
        val endId = context.instrs.length
        context.instrs.append(asm.EndIf(elseId))

        ifInstr.elseInstr = elseId
        elseInstr.endInstr = endId

        dsts.zip(datatypes).zipWithIndex.foreach { case ((src, datatype), idx) =>
          context.data(Data(Some(this), idx)) = (src, datatype)
        }
      case EndIf(_, returnValues) =>
        context.outputDsts.zip(returnValues).foreach {
          case (Some(dst), data) => generateCopyAsm(context, context.datatype(data), dst, context.src(data))
          case (None, _) => ()
        }
      case TupleIdx(tuple, idx) =>
        val (Some(src: asm.DstOff), datatype: TupleDatatype) = context.data(tuple)
        val offset = tupleLayout(datatype.elements)._3(idx)
        datatype.elements(idx) match {
          case UnitDatatype =>
            context.data(Data(this)) = (None, UnitDatatype)
          case tupleDatatype: TupleDatatype =>
            val result = asm.Loc(context.local(tupleDatatype.size, tupleDatatype.align))
            context.data(Data(this)) = (Some(result), tupleDatatype)
            generateCopyAsm(context, tupleDatatype, result, src.offset(offset))
          case subDatatype: Datatype =>
            val result = asm.Reg(context.reg())
            context.data(Data(this)) = (Some(result), subDatatype)
            generateCopyAsm(context, subDatatype, result, src.offset(offset))
        }
      case ReadRef(ref) => ???
      case WriteRef(ref, data) => ???
      case RefValue(data) => ???
      case ReadLocal(local) =>
        context.localTypes(local) match {
          case UnitDatatype => context.data(Data(this)) = (None, UnitDatatype)
          case tupleDatatype: TupleDatatype => context.data(Data(this)) = (Some(asm.Loc(context.localVars(local))), tupleDatatype)
          case subDatatype: Datatype =>
            val result = asm.Reg(context.reg())
            context.data(Data(this)) = (Some(result), subDatatype)
            generateCopyAsm(context, subDatatype, result, asm.Loc(context.localVars(local)))
        }
      case WriteLocal(local, data) =>
        generateCopyAsm(context, context.datatype(data), asm.Loc(context.localVars(local)), context.src(data))
      case RefLocal(local) => ???
      case ReadGlobal(global, idx) => ???
      case WriteGlobal(global, idx, data) => ???
      case RefGlobal(global, idx) => ???
      case Print(data) =>
        context.instrs.append(asm.Print(context.src(data)))
      case Call(Left(fun), values) =>
        // TODO: Tuples that are passed by value should be passed by pointer, callee makes a copy
        // TODO: Tuples that are returned by value should be allocated by caller, then passed by pointer as parameter, no actual returning
        // TODO: Tuple references are treated just like an integer
        val dsts = fun.signature.returnTypes.zipWithIndex.map { case (returnType, idx) =>
          val reg = Some(asm.Reg(context.reg()))
          context.data(Data(Some(this), idx)) = (reg, returnType)
          reg
        }
        context.instrs.append(asm.Call(Left(context.funs(fun)), dsts, values.map(context.src)))
      case Call(Right(fun), values) => ???
      case Ret(returnValues) =>
        context.instrs.append(asm.Ret(returnValues.flatMap(context.srcOption)))
    }
    this match {
      case opNext: OpNext => opNext.next.generateAsm(context)
      case _ => ()
    }
  }
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
case class If(var condition: Data, var ifBlock: Block, var elseBlock: Block, var datatypes: List[Datatype]) extends OpNext
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
case class Print(var data: Data) extends OpNext
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

  def generateAsm(): asm.Program = {
    val context = new AbstractAsmContext(this)

    val asmFuns = funs.map { fun =>
      val (block, (params, returnValues)) = context.fun(fun.localVars) {
        fun.signature.params.zipWithIndex.foreach {
          case (UnitDatatype, _) => ()
          // case (TupleDatatype(elements), idx) => context.data(Data(None, idx)) = (Some(asm.Mem(context.reg())), TupleDatatype(elements))
          case (datatype, idx) => context.data(Data(None, idx)) = (Some(asm.Par(context.param())), datatype)
        }

        val params = context.regCount

        val returnValues = fun.signature.returnTypes.count {
          case UnitDatatype => false
          case _ => true
        }

        fun.next.generateAsm(context)

        (params, returnValues)
      }
      new asm.Fun(block, params, returnValues, context.regCount, context.localCount)
    }.toArray

    new asm.Program(asmFuns, Array.empty)
  }
}

object OptUnit {
  var ids: Long = 0

  def nextId: Long = {
    ids += 1
    ids
  }
}
