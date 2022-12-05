package opt

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.ops.int

object ConstExpr {
  def unapply(data: Data): Option[ConstVal] = data match {
    case Data(Some(UnitLit()), 0) => Some(ConstUnit)
    case Data(Some(IntLit(int)), 0) => Some(ConstInt(int))
    case Data(Some(BoolLit(bool)), 0) => Some(ConstBool(bool))
    case Data(Some(FunLit(fun)), 0) => Some(ConstFun(fun))
    case _ => None
  }
}

object ViewCtrl {
  def unapply(op: Op): Option[(Ctrl, Op)] = Option(op.next).map((_, op))
}

class NodeVisitorValue {
  val visited: mutable.Set[Long] = mutable.Set[Long]()

  @inline
  def apply[T](op: Op)(f: Op => Option[T]): Option[T] = if (visited.contains(op.id)) {
    None
  } else {
    visited.add(op.id)
    f(op)
  }
}

object NodeVisitorValue {
  def apply(): NodeVisitorValue = new NodeVisitorValue()
}

class NodeVisitorUnit {
  val visited: mutable.Set[Long] = mutable.Set[Long]()

  @inline
  def apply(op: Op)(f: Op => Unit): Unit = if (visited.contains(op.id)) {
    ()
  } else {
    visited.add(op.id)
    f(op)
  }
}

object NodeVisitorUnit {
  def apply(): NodeVisitorUnit = new NodeVisitorUnit()
}

def mapDataflow(dataflowMap: mutable.Map[Data, Data])(data: Data): Data = dataflowMap.getOrElse(data, data)

def replaceDataflow(visit: NodeVisitorUnit, dataflowMap: mutable.Map[Data, Data], op: Op): Unit = visit(op) {
  case tupleLit@TupleLit(elements) =>
    tupleLit.elements = elements.map(mapDataflow(dataflowMap))
    replaceDataflow(visit, dataflowMap, op.next)
  case addInt@AddInt(addInts, subInts) =>
    addInt.addInts = addInts.map(mapDataflow(dataflowMap))
    addInt.subInts = subInts.map(mapDataflow(dataflowMap))
    replaceDataflow(visit, dataflowMap, op.next)
  case andInt@AndInt(ints) =>
    andInt.ints = ints.map(mapDataflow(dataflowMap))
    replaceDataflow(visit, dataflowMap, op.next)
  case orInt@OrInt(ints) =>
    orInt.ints = ints.map(mapDataflow(dataflowMap))
    replaceDataflow(visit, dataflowMap, op.next)
  case xorInt@XorInt(ints) =>
    xorInt.ints = ints.map(mapDataflow(dataflowMap))
    replaceDataflow(visit, dataflowMap, op.next)
  case multInt@MultInt(ints) =>
    multInt.ints = ints.map(mapDataflow(dataflowMap))
    replaceDataflow(visit, dataflowMap, op.next)
  case divInt@DivInt(dividend, divisor) =>
    divInt.dividend = mapDataflow(dataflowMap)(dividend)
    divInt.divisor = mapDataflow(dataflowMap)(divisor)
    replaceDataflow(visit, dataflowMap, op.next)
  case modInt@ModInt(dividend, divisor) =>
    modInt.dividend = mapDataflow(dataflowMap)(dividend)
    modInt.divisor = mapDataflow(dataflowMap)(divisor)
    replaceDataflow(visit, dataflowMap, op.next)
  case isGreater@IsGreater(int) =>
    isGreater.int = mapDataflow(dataflowMap)(int)
    replaceDataflow(visit, dataflowMap, op.next)
  case isGreaterOrZero@IsGreaterOrZero(int) =>
    isGreaterOrZero.int = mapDataflow(dataflowMap)(int)
    replaceDataflow(visit, dataflowMap, op.next)
  case isZero@IsZero(int) =>
    isZero.int = mapDataflow(dataflowMap)(int)
    replaceDataflow(visit, dataflowMap, op.next)
  case isNotZero@IsNotZero(int) =>
    isNotZero.int = mapDataflow(dataflowMap)(int)
    replaceDataflow(visit, dataflowMap, op.next)
  case branch@Branch(condition, elseNext) =>
    branch.condition = mapDataflow(dataflowMap)(condition)
    replaceDataflow(visit, dataflowMap, op.next)
    replaceDataflow(visit, dataflowMap, op.next)
    replaceDataflow(visit, dataflowMap, elseNext)
  case phi@Phi(_, ifTrue, ifFalse) =>
    phi.ifTrue = mapDataflow(dataflowMap)(ifTrue)
    phi.ifFalse = mapDataflow(dataflowMap)(ifFalse)
    replaceDataflow(visit, dataflowMap, op.next)
  case tupleIdx@TupleIdx(tuple, _) =>
    tupleIdx.tuple = mapDataflow(dataflowMap)(tuple)
    replaceDataflow(visit, dataflowMap, op.next)
  case readRef@ReadRef(ref) =>
    readRef.ref = mapDataflow(dataflowMap)(ref)
    replaceDataflow(visit, dataflowMap, op.next)
  case writeRef@WriteRef(ref, data) =>
    writeRef.ref = mapDataflow(dataflowMap)(ref)
    writeRef.data = mapDataflow(dataflowMap)(data)
    replaceDataflow(visit, dataflowMap, op.next)
  case writeLocal@WriteLocal(_, data) =>
    writeLocal.data = mapDataflow(dataflowMap)(data)
    replaceDataflow(visit, dataflowMap, op.next)
  case writeGlobal@WriteGlobal(_, _, data) =>
    writeGlobal.data = mapDataflow(dataflowMap)(data)
    replaceDataflow(visit, dataflowMap, op.next)
  case call@Call(fun, values) =>
    call.fun = fun.map(mapDataflow(dataflowMap))
    call.values = values.map(mapDataflow(dataflowMap))
    replaceDataflow(visit, dataflowMap, op.next)
  case Ret(_) => ()
  case _ => replaceDataflow(visit, dataflowMap, op.next)
}

def isPure(op: Op): Boolean = op match {
  case UnitLit() | IntLit(_) | BoolLit(_) | FunLit(_) | TupleLit(_) | AddInt(_, _) | AndInt(_) | OrInt(_) | XorInt(_) | MultInt(_) | DivInt(_, _) | ModInt(_, _)
       | IsGreater(_) | IsGreaterOrZero(_) | IsZero(_) | IsNotZero(_) | TupleIdx(_, _) | ReadRef(_) | RefValue(_) | ReadLocal(_) | RefLocal(_) | ReadGlobal(_, _) | RefGlobal(_, _) => true
  case Branch(_, _) | Phi(_, _, _) | WriteRef(_, _) | WriteLocal(_, _) | WriteGlobal(_, _, _) | Call(_, _) | Ret(_) => false
}

def globalVarInline(optUnit: OptUnit): Boolean = {
  def constFoldGlobal(visit: NodeVisitorValue, globalVar: Var, op: Op): Option[ConstVal] = visit(op) {
    case ViewCtrl(Ret(_), WriteGlobal(global, 0, ConstExpr(constVal))) if globalVar == global => Some(constVal)
    case Branch(_, elseNext) => constFoldGlobal(visit, globalVar, op.next).orElse(constFoldGlobal(visit, globalVar, elseNext))
    case Ret(_) => None
    case _ => constFoldGlobal(visit, globalVar, op.next)
  }

  val constGlobals = (for {
    globalVar <- optUnit.vars
    constVal <- constFoldGlobal(NodeVisitorValue(), globalVar, globalVar.entry)
  } yield (globalVar.id, constVal)).toMap

  def replaceExpr(visit: NodeVisitorUnit, dataflowMap: mutable.Map[Data, Data], op: Op): Unit = visit(op) {
    case readGlobal@ReadGlobal(global, 0) if constGlobals.contains(global.id) =>
      val (firstConstOp: Op, lastConstOp: Op) = constGlobals(global.id).toGraph
      lastConstOp.next = readGlobal.next
      readGlobal.next = firstConstOp
      dataflowMap(Data(readGlobal)) = Data(lastConstOp)
      replaceExpr(visit, dataflowMap, op.next)
    case Branch(_, elseNext) =>
      replaceExpr(visit, dataflowMap, op.next)
      replaceExpr(visit, dataflowMap, elseNext)
    case Ret(_) => None
    case _ => replaceExpr(visit, dataflowMap, op.next)
  }

  optUnit.funs.foreach {
    case codeFun: CodeFun =>
      val dataflowMap = mutable.Map[Data, Data]()
      replaceExpr(NodeVisitorUnit(), dataflowMap, codeFun.entry)
      replaceDataflow(NodeVisitorUnit(), dataflowMap, codeFun.entry)
    case _ => ()
  }

  false
}

def funExprInline(optUnit: OptUnit): Boolean = {
  def callInline(visit: NodeVisitorUnit, op: Op): Unit = visit(op) {
    case call@Call(Right(Data(Some(FunLit(fun)), 0)), _) =>
      call.fun = Left(fun)
      callInline(visit, call.next)
    case Branch(_, elseNext) =>
      callInline(visit, op.next)
      callInline(visit, elseNext)
    case Ret(_) => ()
    case _ => callInline(visit, op.next)
  }

  optUnit.funs.foreach {
    case codeFun: CodeFun => callInline(NodeVisitorUnit(), codeFun.entry)
    case _ => ()
  }

  optUnit.vars.foreach { globalVar =>
    callInline(NodeVisitorUnit(), globalVar.entry)
  }

  false
}

def localVarInline(optUnit: OptUnit): Boolean = {
  def findSingleAssignLocals(visit: NodeVisitorUnit, locals: mutable.Map[Int, Data], multipleAssign: mutable.Set[Int], op: Op): Unit = visit(op) {
    case WriteLocal(local, data) if locals.contains(local) =>
      locals.remove(local)
      multipleAssign.add(local)
      findSingleAssignLocals(visit, locals, multipleAssign, op.next)
    case WriteLocal(local, data) if !multipleAssign.contains(local) =>
      locals(local) = data
      findSingleAssignLocals(visit, locals, multipleAssign, op.next)
    case Branch(_, elseNext) =>
      findSingleAssignLocals(visit, locals, multipleAssign, op.next)
      findSingleAssignLocals(visit, locals, multipleAssign, elseNext)
    case Ret(_) => ()
    case _ => findSingleAssignLocals(visit, locals, multipleAssign, op.next)
  }

  def replaceSingleAssignLocals(visit: NodeVisitorUnit, locals: mutable.Map[Int, Data], dataflowMap: mutable.Map[Data, Data], op: Op): Unit = visit(op) {
    case branch@Branch(_, writeOp@WriteLocal(local, _)) if locals.contains(local) =>
      branch.elseNext = writeOp.next
      visit.visited.remove(op.id)
      replaceSingleAssignLocals(visit, locals, dataflowMap, op)
    case ViewCtrl(writeOp@WriteLocal(local, _), op) if locals.contains(local) =>
      op.next = writeOp.next
      visit.visited.remove(op.id)
      replaceSingleAssignLocals(visit, locals, dataflowMap, op)
    case Branch(_, elseNext) =>
      replaceSingleAssignLocals(visit, locals, dataflowMap, op.next)
      replaceSingleAssignLocals(visit, locals, dataflowMap, elseNext)
    case ReadLocal(local) =>
      dataflowMap(Data(op)) = locals(local)
      replaceSingleAssignLocals(visit, locals, dataflowMap, op.next)
    case RefLocal(local) =>
      val refOp = RefValue(locals(local))
      refOp.next = op.next
      op.next = refOp
      dataflowMap(Data(op)) = Data(refOp)
      replaceSingleAssignLocals(visit, locals, dataflowMap, op.next)
    case Ret(_) => ()
    case _ => replaceSingleAssignLocals(visit, locals, dataflowMap, op.next)
  }

  optUnit.funs.foreach {
    case codeFun: CodeFun =>
      val locals = mutable.Map[Int, Data]()
      val dataflowMap = mutable.Map[Data, Data]()
      findSingleAssignLocals(NodeVisitorUnit(), locals, mutable.Set(), codeFun.entry)
      replaceSingleAssignLocals(NodeVisitorUnit(), locals, dataflowMap, codeFun.entry)
      replaceDataflow(NodeVisitorUnit(), dataflowMap, codeFun.entry)
    case _ => ()
  }

  optUnit.vars.foreach { globalVar =>
    val locals = mutable.Map[Int, Data]()
    val dataflowMap = mutable.Map[Data, Data]()
    findSingleAssignLocals(NodeVisitorUnit(), locals, mutable.Set(), globalVar.entry)
    replaceSingleAssignLocals(NodeVisitorUnit(), locals, dataflowMap, globalVar.entry)
    replaceDataflow(NodeVisitorUnit(), dataflowMap, globalVar.entry)
  }

  false
}

def inlineFunctions(optUnit: OptUnit): Boolean = {
  val INLINE_THRESHOLD = 15

  val functionsToInline = mutable.Map[CodeFun, Int]()

  def iterateNodes(visit: NodeVisitorUnit, op: Op): Unit = visit(op) {
    case Branch(_, elseNext) =>
      iterateNodes(visit, op.next)
      iterateNodes(visit, elseNext)
    case Ret(_) => ()
    case _ => iterateNodes(visit, op.next)
  }

  def shouldInline(codeFun: CodeFun): Boolean = functionsToInline.getOrElseUpdate(codeFun, {
    val visit = NodeVisitorUnit()
    iterateNodes(visit, codeFun.entry)
    visit.visited.size
  }) <= INLINE_THRESHOLD

  def inlineFunction(visit: NodeVisitorUnit, mainFun: Option[CodeFun], codeFun: CodeFun, op: Op): Unit = visit(op) {
    case _ => ???
  }

  def scanForInline(visit: NodeVisitorUnit, codeFun: Option[CodeFun], op: Op): Unit = visit(op) {
    case ViewCtrl(Call(Left(codeFun: CodeFun), params), _) => ???
    case Branch(_, elseNext) =>
      iterateNodes(visit, op.next)
      iterateNodes(visit, elseNext)
    case Ret(_) => ()
    case _ => iterateNodes(visit, op.next)
  }

  false
}

def deadCodeElimination(optUnit: OptUnit): Boolean = {
  def findReachableNodes(visit: NodeVisitorUnit, reachable: mutable.Set[Long], op: Op): Unit = visit(op) {
    case TupleLit(elements) =>
      reachable.addAll(elements.flatMap(_._1).map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case AddInt(addInts, subInts) =>
      reachable.addAll(addInts.flatMap(_._1).map(_.id))
      reachable.addAll(subInts.flatMap(_._1).map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case AndInt(ints) =>
      reachable.addAll(ints.flatMap(_._1).map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case OrInt(ints) =>
      reachable.addAll(ints.flatMap(_._1).map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case XorInt(ints) =>
      reachable.addAll(ints.flatMap(_._1).map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case MultInt(ints) =>
      reachable.addAll(ints.flatMap(_._1).map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case DivInt(dividend, divisor) =>
      reachable.addAll(dividend._1.map(_.id))
      reachable.addAll(divisor._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case ModInt(dividend, divisor) =>
      reachable.addAll(dividend._1.map(_.id))
      reachable.addAll(divisor._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case IsGreater(int) =>
      reachable.addAll(int._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case IsGreaterOrZero(int) =>
      reachable.addAll(int._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case IsZero(int) =>
      reachable.addAll(int._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case IsNotZero(int) =>
      reachable.addAll(int._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case Branch(condition, elseNext) =>
      reachable.addAll(condition._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
      findReachableNodes(visit, reachable, elseNext)
    case Phi(_, ifTrue, ifFalse) =>
      reachable.addAll(ifTrue._1.map(_.id))
      reachable.addAll(ifFalse._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case TupleIdx(tuple, _) =>
      reachable.addAll(tuple._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case ReadRef(ref) =>
      reachable.addAll(ref._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case WriteRef(ref, data) =>
      reachable.addAll(ref._1.map(_.id))
      reachable.addAll(data._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case WriteLocal(_, data) =>
      reachable.addAll(data._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case WriteGlobal(_, _, data) =>
      reachable.addAll(data._1.map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case Call(fun, values) =>
      reachable.addAll(fun.toOption.flatMap(_._1).map(_.id))
      reachable.addAll(values.flatMap(_._1).map(_.id))
      findReachableNodes(visit, reachable, op.next)
    case Ret(returnValues) =>
      reachable.addAll(returnValues.flatMap(_._1).map(_.id))
    case _ => findReachableNodes(visit, reachable, op.next)
  }

  def purgeUnreachableNodes(visit: NodeVisitorUnit, reachable: mutable.Set[Long], op: Op): Unit = visit(op) {
    case branch@Branch(_, elseNext) if !reachable.contains(elseNext.id) && isPure(elseNext) =>
      branch.elseNext = elseNext.next
      visit.visited.remove(op.id)
      purgeUnreachableNodes(visit, reachable, op)
    case ViewCtrl(nextOp, op) if !reachable.contains(nextOp.id) && isPure(nextOp) =>
      op.next = nextOp.next
      visit.visited.remove(op.id)
      purgeUnreachableNodes(visit, reachable, op)
    case Branch(_, elseNext) =>
      purgeUnreachableNodes(visit, reachable, op.next)
      purgeUnreachableNodes(visit, reachable, elseNext)
    case Ret(_) => ()
    case _ => purgeUnreachableNodes(visit, reachable, op.next)
  }

  optUnit.funs.foreach {
    case codeFun: CodeFun =>
      val reachable = mutable.Set[Long]()
      findReachableNodes(NodeVisitorUnit(), reachable, codeFun.entry)
      purgeUnreachableNodes(NodeVisitorUnit(), reachable, codeFun.entry)
    case _ => ()
  }

  optUnit.vars.foreach { globalVar =>
    val reachable = mutable.Set[Long]()
    findReachableNodes(NodeVisitorUnit(), reachable, globalVar.entry)
    purgeUnreachableNodes(NodeVisitorUnit(), reachable, globalVar.entry)
  }

  false
}
