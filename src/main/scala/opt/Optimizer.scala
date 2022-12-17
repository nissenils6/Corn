package opt

import sem.GlobalVar

import scala.annotation.tailrec
import scala.collection.mutable

object ConstExpr {
  def unapply(data: Data): Option[ConstVal] = data match {
    case Data(Some(UnitLit()), 0) => Some(ConstUnit)
    case Data(Some(IntLit(int)), 0) => Some(ConstInt(int))
    case Data(Some(BoolLit(bool)), 0) => Some(ConstBool(bool))
    case Data(Some(FunLit(fun)), 0) => Some(ConstFun(fun))
    case _ => None
  }
}

def visitOps(prev: HasNext)(f: Op => Unit): Unit = {
  var cur = prev

  while (true) {
    f(cur.next)
    cur.next match {
      case ifNode@If(_, ifBlock, elseBlock) =>
        visitOps(ifBlock)(f)
        visitOps(elseBlock)(f)
        cur = ifNode
      case opNext: OpNext => cur = opNext
      case _ => return
    }
  }
}

def visitOpsAndPrev(prev: HasNext)(f: (HasNext, Op) => Boolean): Unit = {
  var cur = prev

  while (true) {
    val revisit = f(cur, cur.next)
    cur.next match {
      case ifNode@If(_, ifBlock, elseBlock) =>
        visitOpsAndPrev(ifBlock)(f)
        visitOpsAndPrev(elseBlock)(f)
        if (!revisit) cur = ifNode
      case opNext: OpNext => if (!revisit) cur = opNext
      case _ => return
    }
  }
}

def visitOpsAndPrevAcc[T](prev: HasNext, acc: T)(f: (HasNext, Op, T) => T): T = {
  var cur = prev
  var curAcc = acc

  while (true) cur.next match {
    case ifNode@If(_, ifBlock, elseBlock) =>
      curAcc = f(cur, ifNode, curAcc)
      curAcc = visitOpsAndPrevAcc(ifBlock, curAcc)(f)
      curAcc = visitOpsAndPrevAcc(elseBlock, curAcc)(f)
      cur = ifNode
    case opNext: OpNext =>
      curAcc = f(cur, opNext, curAcc)
      cur = opNext
    case op =>
      return f(cur, op, curAcc)
  }

  curAcc
}

def visitOpsAndPrevUntil[T](prev: HasNext)(f: (HasNext, Op) => Option[T]): Option[T] = {
  var cur = prev

  while (true) cur.next match {
    case ifNode@If(_, ifBlock, elseBlock) =>
      f(cur, ifNode).orElse(
        visitOpsAndPrevUntil(ifBlock)(f)
      ).orElse(
        visitOpsAndPrevUntil(elseBlock)(f)
      ) match {
        case Some(value) => return Some(value)
        case None => ()
      }
      cur = ifNode
    case opNext: OpNext =>
      f(cur, opNext) match {
        case Some(value) => return Some(value)
        case None => ()
      }
      cur = opNext
    case op =>
      return f(cur, op)
  }
  None
}

def visitData(prev: HasNext)(f: Data => Unit): Unit = visitOps(prev) {
  case _: UnitLit | _: IntLit | _: BoolLit | _: FunLit | _: ReadLocal | _: RefLocal | _: ReadGlobal | _: RefGlobal => ()
  case node: TupleLit => node.elements.foreach(f)
  case node: AddInt =>
    node.addInts.foreach(f)
    node.subInts.foreach(f)
  case node: BitwiseInt =>
    node.ints.foreach(f)
  case node: MultInt =>
    node.ints.foreach(f)
  case node: DivInt =>
    f(node.dividend)
    f(node.divisor)
  case node: CompInt =>
    f(node.int)
  case node: If =>
    f(node.condition)
  case node: EndIf =>
    node.returnValues.foreach(f)
  case node: TupleIdx =>
    f(node.tuple)
  case node: ReadRef =>
    f(node.ref)
  case node: WriteRef =>
    f(node.ref)
    f(node.data)
  case node: RefValue =>
    f(node.data)
  case node: WriteLocal =>
    f(node.data)
  case node: WriteGlobal =>
    f(node.data)
  case node: PrintI64 =>
    f(node.data)
  case node: Call =>
    node.fun.foreach(f)
    node.values.foreach(f)
  case node: Ret =>
    node.returnValues.foreach(f)
}

def visitAndMapData(prev: HasNext)(f: Data => Data): Unit = visitOps(prev) {
  case _: UnitLit | _: IntLit | _: BoolLit | _: FunLit | _: ReadLocal | _: RefLocal | _: ReadGlobal | _: RefGlobal => ()
  case node: TupleLit =>
    node.elements = node.elements.map(f)
  case node: AddInt =>
    node.addInts = node.addInts.map(f)
    node.subInts = node.subInts.map(f)
  case node: BitwiseInt =>
    node.ints = node.ints.map(f)
  case node: MultInt =>
    node.ints = node.ints.map(f)
  case node: DivInt =>
    node.dividend = f(node.dividend)
    node.divisor = f(node.divisor)
  case node: CompInt =>
    node.int = f(node.int)
  case node: If =>
    node.condition = f(node.condition)
  case node: EndIf =>
    node.returnValues = node.returnValues.map(f)
  case node: TupleIdx =>
    node.tuple = f(node.tuple)
  case node: ReadRef =>
    node.ref = f(node.ref)
  case node: WriteRef =>
    node.ref = f(node.ref)
    node.data = f(node.data)
  case node: RefValue =>
    node.data = f(node.data)
  case node: WriteLocal =>
    node.data = f(node.data)
  case node: WriteGlobal =>
    node.data = f(node.data)
  case node: PrintI64 =>
    node.data = f(node.data)
  case node: Call =>
    node.fun = node.fun.map(f)
    node.values = node.values.map(f)
  case node: Ret =>
    node.returnValues = node.returnValues.map(f)
}

@tailrec
def mapDataflow(dataflowMap: collection.Map[Data, Data])(data: Data): Data = if dataflowMap.contains(data) then mapDataflow(dataflowMap)(dataflowMap(data)) else data

def isPure(op: Op): Boolean = op match {
  case UnitLit() | IntLit(_) | BoolLit(_) | FunLit(_) | TupleLit(_) | AddInt(_, _) | BitwiseInt(_, _) | MultInt(_) | DivInt(_, _)
       | CompInt(_, _) | TupleIdx(_, _) | ReadRef(_) | RefValue(_) | ReadLocal(_) | RefLocal(_) | ReadGlobal(_, _) | RefGlobal(_, _) => true
  case If(_, _, _) | EndIf(_, _) | WriteRef(_, _) | WriteLocal(_, _) | WriteGlobal(_, _, _) | PrintI64(_) | Call(_, _) | Ret(_) => false
}

def globalVarInline(optUnit: OptUnit): Boolean = {
  val constGlobals = (for {
    globalVar <- optUnit.vars
    constVal <- visitOpsAndPrevUntil(globalVar) {
      case (WriteGlobal(global, 0, ConstExpr(constVal)), Ret(_)) if globalVar == global => Some(constVal)
      case _ => None
    }
  } yield (globalVar, constVal)).toMap
  
  optUnit.funs.foreach { fun =>
    val dataflowMap = mutable.Map[Data, Data]()
    visitOps(fun) {
      case readGlobal@ReadGlobal(global, 0) if constGlobals.contains(global) =>
        val (firstConstOp: Op, lastConstOp: OpNext) = constGlobals(global).toGraph
        lastConstOp.next = readGlobal.next
        readGlobal.next = firstConstOp
        dataflowMap(Data(readGlobal)) = Data(lastConstOp)
      case _ => ()
    }
    visitAndMapData(fun)(mapDataflow(dataflowMap))
  }

  false
}

def funExprInline(optUnit: OptUnit): Boolean = {
  def callInline(hasNext: HasNext): Unit = visitOps(hasNext) {
    case call@Call(Right(Data(Some(FunLit(fun)), 0)), _) => call.fun = Left(fun)
    case _ => ()
  }

  optUnit.funs.foreach(callInline)
  optUnit.vars.foreach(callInline)

  false
}

def localVarInline(optUnit: OptUnit): Boolean = {
  def findSingleAssignLocals(hasNext: HasNext, locals: mutable.Map[Int, Data], multipleAssign: mutable.Set[Int]): Unit = visitOps(hasNext) {
    case WriteLocal(local, data) if !multipleAssign.contains(local) =>
      locals(local) = data
    case WriteLocal(local, _) if locals.contains(local) =>
      locals.remove(local)
      multipleAssign.add(local)
    case RefLocal(local) if locals.contains(local) =>
      locals.remove(local)
      multipleAssign.add(local)
    case _ => ()
  }

  def replaceSingleAssignLocals(hasNext: HasNext, locals: mutable.Map[Int, Data], dataflowMap: mutable.Map[Data, Data]): Unit = visitOpsAndPrev(hasNext) {
    case (op, writeOp@WriteLocal(local, _)) if locals.contains(local) =>
      op.next = writeOp.next
      true
    case (op, readOp@ReadLocal(local)) if locals.contains(local) =>
      dataflowMap(Data(readOp)) = locals(local)
      op.next = readOp.next
      true
    case _ => false
  }

  optUnit.funs.foreach { fun =>
    val locals = mutable.Map[Int, Data]()
    val dataflowMap = mutable.Map[Data, Data]()
    findSingleAssignLocals(fun, locals, mutable.Set())
    replaceSingleAssignLocals(fun, locals, dataflowMap)
    visitAndMapData(fun)(mapDataflow(dataflowMap))
  }

  optUnit.vars.foreach { globalVar =>
    val locals = mutable.Map[Int, Data]()
    val dataflowMap = mutable.Map[Data, Data]()
    findSingleAssignLocals(globalVar, locals, mutable.Set())
    replaceSingleAssignLocals(globalVar, locals, dataflowMap)
    visitAndMapData(globalVar)(mapDataflow(dataflowMap))
  }

  false
}

def inlineFunctions(optUnit: OptUnit): Boolean = {
  val INLINE_THRESHOLD = 15

  val functionsToInline = mutable.Map[Fun, Int]()

  def shouldInline(fun: Fun): Boolean = functionsToInline.getOrElseUpdate(fun, visitOpsAndPrevAcc(fun, 0) {
    case (_, _, i) => i + 1
  }) <= INLINE_THRESHOLD

  def gatherFunctions(hasNext: HasNext, funs: mutable.Set[Fun]): Unit = visitOps(hasNext) {
    case Call(Left(fun: Fun), _) => funs.add(fun)
    case _ => ()
  }
  
  def inlineFunction(params: Map[Int, Data], opMap: mutable.Map[Long, Op], newLocalsOffset: Int, newRetsOffset: Int, nextCtrl: Ctrl, hasNext: HasNext): Op = {
    def recur(newHasNext: HasNext) = inlineFunction(params, opMap, newLocalsOffset, newRetsOffset, nextCtrl, newHasNext)

    def mapData(data: Data) = data match {
      case Data(Some(op), idx) => Data(Some(opMap(op.id)), idx)
      case Data(None, idx) => params(idx)
    }

    def newNode(oldOp: OpNext, newOp: OpNext): Op = {
      opMap(oldOp.id) = newOp
      newOp.next = recur(oldOp)
      newOp
    }

    hasNext.next match {
      case oldOp@UnitLit() => newNode(oldOp, UnitLit())
      case oldOp@IntLit(int) => newNode(oldOp, IntLit(int))
      case oldOp@BoolLit(bool) => newNode(oldOp, BoolLit(bool))
      case oldOp@FunLit(fun) => newNode(oldOp, FunLit(fun))
      case oldOp@TupleLit(elements) => newNode(oldOp, TupleLit(elements.map(mapData)))
      case oldOp@AddInt(addInts, subInts) => newNode(oldOp, AddInt(addInts.map(mapData), subInts.map(mapData)))
      case oldOp@BitwiseInt(bitwiseOp, ints) => newNode(oldOp, BitwiseInt(bitwiseOp, ints.map(mapData)))
      case oldOp@MultInt(ints) => newNode(oldOp, MultInt(ints.map(mapData)))
      case oldOp@DivInt(dividend, divisor) => newNode(oldOp, DivInt(mapData(dividend), mapData(divisor)))
      case oldOp@CompInt(compType, int) => newNode(oldOp, CompInt(compType, mapData(int)))
      case oldOp@If(condition, ifBlock, elseBlock) => newNode(oldOp, If(mapData(condition), Block(recur(ifBlock)), Block(recur(elseBlock))))
      case oldOp@TupleIdx(tuple, idx) => newNode(oldOp, TupleIdx(mapData(tuple), idx))
      case oldOp@ReadRef(ref) => newNode(oldOp, ReadRef(mapData(ref)))
      case oldOp@WriteRef(ref, data) => newNode(oldOp, WriteRef(mapData(ref), mapData(data)))
      case oldOp@RefValue(data) => newNode(oldOp, RefValue(mapData(data)))
      case oldOp@ReadLocal(local) => newNode(oldOp, ReadLocal(local + newLocalsOffset))
      case oldOp@WriteLocal(local, data) => newNode(oldOp, WriteLocal(local + newLocalsOffset, mapData(data)))
      case oldOp@RefLocal(local) => newNode(oldOp, RefLocal(local + newLocalsOffset))
      case oldOp@ReadGlobal(global, idx) => newNode(oldOp, ReadGlobal(global, idx))
      case oldOp@WriteGlobal(global, idx, data) => newNode(oldOp, WriteGlobal(global, idx, mapData(data)))
      case oldOp@RefGlobal(global, idx) => newNode(oldOp, RefGlobal(global, idx))
      case oldOp@PrintI64(data) => newNode(oldOp, PrintI64(mapData(data)))
      case oldOp@Call(Left(fun), values) => newNode(oldOp, Call(Left(fun), values.map(mapData)))
      case oldOp@Call(Right(fun), values) => newNode(oldOp, Call(Right(mapData(fun)), values.map(mapData)))
      case Ret(returnValues) =>
        val returnValueOps = returnValues.zipWithIndex.map { case (data, idx) =>
          WriteLocal(newRetsOffset + idx, mapData(data))
        }
        linkOps(nextCtrl)(returnValueOps)
    }
  }

  val funCalling = optUnit.funs.map { fun =>
    val funs = mutable.Set[Fun]()
    gatherFunctions(fun, funs)
    (fun, funs.toList)
  }.toMap

  def reaching(reachingSet: mutable.Set[Fun])(fun: Fun): Unit = if (!reachingSet.contains(fun)) {
    reachingSet.add(fun)
    funCalling(fun).foreach(reaching(reachingSet))
  }

  val funReaching = funCalling.keys.map { fun =>
    val reachingSet = mutable.Set[Fun]()
    funCalling(fun).foreach(reaching(reachingSet))
    (fun, reachingSet.toList)
  }.toMap

  def scanForInline(hasNext: HasNext, body: Either[Var, Fun]): Unit = visitOpsAndPrev(hasNext) {
    case (prev, callOp@Call(Left(fun: Fun), params)) if !funReaching(fun).contains(fun) && shouldInline(fun) =>
      val newLocalsOffset = body match {
        case Left(globalVar) => globalVar.localVars.length
        case Right(mainFun) => mainFun.localVars.length
      }
      val newRetsOffset = newLocalsOffset + fun.localVars.length
      val unitOp = UnitLit()
      prev.next = inlineFunction(params.zipWithIndex.map(t => (t._2, t._1)).toMap, mutable.Map.empty, newLocalsOffset, newRetsOffset, unitOp, fun)
      val retValCount = fun.signature.returnTypes.length
      val (retLocalOps, retValueMap) = Range(0, retValCount).map { i =>
        val readLocalOp = ReadLocal(newRetsOffset + i)
        (readLocalOp, (Data(Some(callOp), i), Data(Some(readLocalOp), 0)))
      }.toList.unzip
      val firstRetLocalOp = linkOps(callOp.next)(retLocalOps)
      unitOp.next = firstRetLocalOp
      body match {
        case Left(globalVar) =>
          globalVar.localVars = globalVar.localVars ::: fun.localVars ::: fun.signature.returnTypes
          visitAndMapData(globalVar)(mapDataflow(retValueMap.toMap))
        case Right(mainFun) =>
          mainFun.localVars = mainFun.localVars ::: fun.localVars ::: fun.signature.returnTypes
          functionsToInline.remove(mainFun)
          visitAndMapData(mainFun)(mapDataflow(retValueMap.toMap))
      }
      true
    case _ => false
  }

  optUnit.vars.foreach { globalVar =>
    scanForInline(globalVar, Left(globalVar))
  }

  optUnit.funs.foreach { fun =>
    scanForInline(fun, Right(fun))
  }

  false
}

def deadCodeElimination(optUnit: OptUnit): Boolean = {
  def findReachableNodes(hasNext: HasNext, reachable: mutable.Set[Long]): Unit = visitData(hasNext) {
    case Data(Some(op), _) => reachable.add(op.id)
    case _ => ()
  }

  def purgeUnreachableNodes(hasNext: HasNext, reachable: mutable.Set[Long]): Unit = visitOpsAndPrev(hasNext) {
    case (prev, opNext: OpNext) if !reachable.contains(opNext.id) && isPure(opNext) =>
      prev.next = opNext.next
      true
    case (_, op) => false
  }

  optUnit.funs.foreach { fun =>
    val reachable = mutable.Set[Long]()
    findReachableNodes(fun, reachable)
    purgeUnreachableNodes(fun, reachable)
  }

  optUnit.vars.foreach { globalVar =>
    val reachable = mutable.Set[Long]()
    findReachableNodes(globalVar, reachable)
    purgeUnreachableNodes(globalVar, reachable)
  }

  false
}

def deadBlockElimination(optUnit: OptUnit): Boolean = {
  val gatheredFuns = mutable.Set[Fun]()
  val gatheredVars = mutable.Set[Var]()

  def addFun(fun: Fun): Unit = if (!gatheredFuns.contains(fun)) {
    gatheredFuns.add(fun)
    gatherBlocks(fun)
  }

  def addVar(globalVar: Var): Unit = if (!gatheredVars.contains(globalVar)) {
    gatheredVars.add(globalVar)
    gatherBlocks(globalVar)
  }

  def gatherBlocks(hasNext: HasNext): Unit = visitOps(hasNext) {
    case FunLit(fun) => addFun(fun)
    case Call(Left(fun), _) => addFun(fun)
    case ReadGlobal(global, _) => addVar(global)
    case WriteGlobal(global, _, _) => addVar(global)
    case RefGlobal(global, _) => addVar(global)
    case _ => ()
  }

  addFun(optUnit.mainFun)

  optUnit.funs = optUnit.funs.filter(gatheredFuns.contains)
  optUnit.vars = optUnit.vars.filter(gatheredVars.contains)

  false
}
