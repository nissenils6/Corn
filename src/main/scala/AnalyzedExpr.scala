import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable

abstract class AnalyzedExpr() {
  def range: FilePosRange

  lazy val returnType: Datatype = this match {
    case AnalyzedCallExpr(function, _, _) => function.returnType.asInstanceOf[FunDatatype].returnType
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.datatype
    case AnalyzedLocalVarRefExpr(localVar, _) => localVar.datatype
    case AnalyzedGlobalFunRefExpr(globalFun, _) => globalFun.signature
    case AnalyzedIntExpr(_, _) => IntDatatype
    case AnalyzedTupleExpr(elements, _) => TupleDatatype(elements.map(_.returnType))
    case AnalyzedBlockExpr(_, lastExpr, _, _) => lastExpr.returnType
    case AnalyzedUnitExpr(_) => UnitDatatype
    case AnalyzedLetExpr(_, expr, _) => expr.returnType
    case AnalyzedFunExpr(fun, _) => fun.signature
    case AnalyzedFunTypeExpr(_, _, _) => TypeDatatype
  }

  lazy val constVal: Option[ConstVal] = this match {
    case AnalyzedCallExpr(function, args, _) => (for {
      fun <- function.constVal
      argValues <- args.map(_.constVal).extract
    } yield fun.asInstanceOf[ConstFunction].function.constEval(argValues)).flatten
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.constVal
    case AnalyzedLocalVarRefExpr(_, range) => throw Error.todo(s"Attempt at compile time evaluating a local variable reference expression outside of a expression evaluation context", range)
    case AnalyzedGlobalFunRefExpr(globalFun, _) => Some(ConstFunction(globalFun))
    case AnalyzedIntExpr(int, _) => Some(ConstInt(int))
    case AnalyzedTupleExpr(elements, _) => elements.map(_.constVal).extract.map(vals => ConstTuple(vals))
    case AnalyzedBlockExpr(_, _, _, _) => constEval(ExprConstEvalContext())
    case AnalyzedUnitExpr(_) => Some(ConstUnit)
    case AnalyzedLetExpr(_, expr, _) => expr.constVal
    case AnalyzedFunExpr(fun, _) => Some(ConstFunction(fun))
    case AnalyzedFunTypeExpr(parameters, returnType, _) => Some(ConstType(FunDatatype(parameters.map(_.constDatatype), returnType.constDatatype)))
  }

  private def mapToConstDatatype(constVal: Option[ConstVal], range: FilePosRange): Datatype = constVal match {
    case Some(ConstType(datatype)) => datatype
    case Some(value) => throw Error.datatypeExpected(value.datatype, range)
    case None => throw Error.datatypeExpected(range)
  }

  lazy val constDatatype: Datatype = this match {
    case AnalyzedCallExpr(function, args, range) => mapToConstDatatype((for {
      fun <- function.constVal
      argValues <- args.map(_.constVal).extract
    } yield fun.asInstanceOf[ConstFunction].function.constEval(argValues)).flatten, range)
    case AnalyzedGlobalVarRefExpr(globalVar, range) => mapToConstDatatype(globalVar.constVal, range)
    case AnalyzedLocalVarRefExpr(_, _) => throw Error.todo(s"Attempt at compile time evaluating a local variable reference expression outside of a expression evaluation context", range)
    case AnalyzedGlobalFunRefExpr(globalFun, range) => throw Error.datatypeExpected(globalFun.signature, range)
    case AnalyzedIntExpr(_, range) => throw Error.datatypeExpected(IntDatatype, range)
    case AnalyzedTupleExpr(elements, _) => TupleDatatype(elements.map(_.constDatatype))
    case AnalyzedBlockExpr(_, _, _, range) => mapToConstDatatype(constEval(ExprConstEvalContext()), range)
    case AnalyzedUnitExpr(_) => UnitDatatype
    case AnalyzedLetExpr(_, expr, range) => mapToConstDatatype(expr.constVal, range)
    case AnalyzedFunExpr(fun, _) => throw Error.datatypeExpected(fun.signature, range)
    case AnalyzedFunTypeExpr(parameters, returnType, _) => FunDatatype(parameters.map(_.constDatatype), returnType.constDatatype)
  }

  def constEval(ctx: ExprConstEvalContext): Option[ConstVal] = this match {
    case AnalyzedCallExpr(function, args, _) => (for {
      fun <- function.constEval(ctx)
      argValues <- args.map(_.constEval(ctx)).extract
    } yield fun.asInstanceOf[ConstFunction].function.constEval(argValues)).flatten
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.constVal
    case AnalyzedLocalVarRefExpr(localVar, _) => Some(ctx.lookup(localVar))
    case AnalyzedGlobalFunRefExpr(globalFun, _) => Some(ConstFunction(globalFun))
    case AnalyzedIntExpr(int, _) => Some(ConstInt(int))
    case AnalyzedTupleExpr(elements, _) => elements.map(element => element.constEval(ctx)).extract.map(vals => ConstTuple(vals))
    case AnalyzedBlockExpr(exprs, lastExpr, _, _) => ctx.scope({
      exprs.foreach(_.constEval(ctx))
      lastExpr.constEval(ctx)
    })
    case AnalyzedUnitExpr(_) => Some(ConstUnit)
    case AnalyzedLetExpr(pattern, expr, _) => expr.constEval(ctx).map(value => ctx.add(pattern, value))
    case AnalyzedFunExpr(fun, _) => Some(ConstFunction(fun))
    case AnalyzedFunTypeExpr(parameters, returnType, _) => Some(ConstType(FunDatatype(parameters.map(_.constDatatype), returnType.constDatatype)))
  }

  def generateCode(ctx: ExprCodeGenContext): Unit = this match {
    case AnalyzedCallExpr(function, args, _) =>
      val signature = function.returnType.asInstanceOf[FunDatatype]

      val argOffset = ctx.secondaryOffset
      function.generateCode(ctx)
      ctx.secondaryOffset -= 8
      ctx.add(
        Load(Reg.RAX, Reg.RBP + ctx.secondaryOffset),
        Store(Reg.RSP + (ctx.offset - 8), Reg.RAX)
      )

      ctx.offset -= 16
      args.foreach(_.generateCode(ctx))
      ctx.offset += 16

      ctx.add(
        Load(Reg.RAX, Reg.RSP + (ctx.offset - 8)),
        Asm.Sub(Reg.RSP, -(ctx.offset - 8)),
        Asm.Add(Reg.RBP, argOffset),
        IndRegCall(Reg.RAX),
        Asm.Sub(Reg.RBP, argOffset),
        Asm.Add(Reg.RSP, -(ctx.offset - 8))
      )
      ctx.secondaryOffset = argOffset + signature.returnType.size.roundUp(8)
    case AnalyzedGlobalVarRefExpr(globalVar, range) =>
      val stackOffset = ctx.secondaryOffset
      ctx.secondaryOffset += globalVar.datatype.size.roundUp(8)
      globalVar.label match {
        case Some(label) => globalVar.datatype.generateCopyCode(ctx, Reg.RBP + stackOffset, Address(label))
        case None => throw Error.todo(range)
      }
    case AnalyzedLocalVarRefExpr(localVar, _) =>
      val stackOffset = ctx.secondaryOffset
      ctx.secondaryOffset += localVar.datatype.size.roundUp(8)
      localVar.datatype.generateCopyCode(ctx, Reg.RBP + stackOffset, Reg.RSP + ctx.lookup(localVar))
    case AnalyzedGlobalFunRefExpr(globalFun, _) =>
      ctx.add(
        Lea(Reg.RAX, Address(globalFun.label)),
        Store(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
      )
      ctx.secondaryOffset += 8
    case AnalyzedIntExpr(int, _) =>
      ctx.add(StoreImm(Reg.RBP + ctx.secondaryOffset, int))
      ctx.secondaryOffset += 8
    case AnalyzedTupleExpr(elements, _) =>
      val stackOffset = ctx.secondaryOffset
      val (tupleSize, _, tupleOffsets) = Datatype.alignSequence(elements.map(_.returnType))
      ctx.secondaryOffset += tupleSize.roundUp(8)
      val elementOffset = ctx.secondaryOffset
      for ((expr, offset) <- elements.zip(tupleOffsets)) {
        expr.generateCode(ctx)
        expr.returnType.generateCopyCode(ctx, Reg.RBP + (stackOffset + offset), Reg.RBP + elementOffset)
        ctx.secondaryOffset = elementOffset
      }
    case AnalyzedBlockExpr(exprs, lastExpr, vars, _) => ctx.scope({
      val varOffset = ctx.offset
      vars.foreach(ctx.add)
      ctx.offset = ctx.offset.roundDown(16)
      exprs.foreach(expr => {
        expr.generateCode(ctx)
        ctx.secondaryOffset -= expr.returnType.size.roundUp(8)
      })
      lastExpr.generateCode(ctx)
      ctx.offset = varOffset
    })
    case AnalyzedUnitExpr(_) => ()
    case AnalyzedLetExpr(pattern, expr, _) =>
      val stackOffset = ctx.secondaryOffset
      expr.generateCode(ctx)

      def iteratePattern(pattern: AnalyzedPattern[LocalVar], offset: Int): Unit = pattern match {
        case AnalyzedVarPattern(patternVar, _) => patternVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.lookup(patternVar), Reg.RBP + offset)
        case AnalyzedTuplePattern(elements, _) => elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3.map(_ + offset)).foreach(iteratePattern.tupled)
      }

      iteratePattern(pattern, stackOffset)
    case AnalyzedFunExpr(fun, _) =>
      ctx.add(
        Lea(Reg.RAX, Address(fun.label)),
        Store(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
      )
      ctx.secondaryOffset += 8
    case AnalyzedFunTypeExpr(_, _, range) => Error.todo("", range)
  }

  def format(indentation: Int): String = this match {
    case AnalyzedCallExpr(function, args, _) => s"${function.format(indentation)}(${args.map(_.format(indentation)).mkString(", ")})"
    case AnalyzedGlobalVarRefExpr(globalVar, _) => s"${globalVar.name}"
    case AnalyzedLocalVarRefExpr(localVar, _) => s"${localVar.name}"
    case AnalyzedGlobalFunRefExpr(globalFun, _) => s"LOL"
    case AnalyzedIntExpr(int, _) => s"$int"
    case AnalyzedTupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case AnalyzedBlockExpr(exprs, lastExpr, _, _) => s"{\n${exprs.map(e => s"${" " * (indentation + 1)}${e.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
    case AnalyzedUnitExpr(_) => s"()"
    case AnalyzedLetExpr(pattern, expr, _) => s"let ${pattern.format(indentation)} = ${expr.format(indentation)}"
    case AnalyzedFunExpr(fun, _) => s"${fun.format(indentation)}"
    case AnalyzedFunTypeExpr(parameters, returnType, _) => s"(${parameters.map(_.format(indentation)).mkString(", ")}) => ${returnType.format(indentation)}"
  }
}

case class AnalyzedCallExpr(function: AnalyzedExpr, args: List[AnalyzedExpr], range: FilePosRange) extends AnalyzedExpr

case class AnalyzedGlobalVarRefExpr(globalVar: GlobalVar, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedLocalVarRefExpr(localVar: LocalVar, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedGlobalFunRefExpr(globalFun: Fun, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedIntExpr(int: Int, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedTupleExpr(elements: List[AnalyzedExpr], range: FilePosRange) extends AnalyzedExpr

case class AnalyzedBlockExpr(exprs: List[AnalyzedExpr], lastExpr: AnalyzedExpr, vars: List[LocalVar], range: FilePosRange) extends AnalyzedExpr

case class AnalyzedUnitExpr(range: FilePosRange) extends AnalyzedExpr

case class AnalyzedLetExpr(pattern: AnalyzedPattern[LocalVar], expr: AnalyzedExpr, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedFunExpr(fun: Fun, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedFunTypeExpr(parameters: List[AnalyzedExpr], returnTypeExpr: AnalyzedExpr, range: FilePosRange) extends AnalyzedExpr

class LocalVar(val module: Module, val name: String, letExpr: => Option[AnalyzedLetExpr], patternNav: PatternNav, typeExpr: Option[Expr], val range: FilePosRange) extends AnalyzerVar {
  lazy val datatype: Datatype = typeExpr.map(typeExpr => analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype).getOrElse(patternNav.datatype(letExpr match {
    case Some(letExpr) => letExpr.expr.returnType
    case None => throw Error.todo(module.file)
  }))
}

abstract class OverloadLayer {
  def apply(funRange: FilePosRange, posArgs: List[AnalyzedExpr], keywordArgs: List[(String, AnalyzedExpr)]): Option[(AnalyzerVar, List[AnalyzedExpr])]
}

case class LocalOverloadLayer(localVar: LocalVar) extends OverloadLayer {
  override def apply(funRange: FilePosRange, posArgs: List[AnalyzedExpr], keywordArgs: List[(String, AnalyzedExpr)]): Option[(AnalyzerVar, List[AnalyzedExpr])] = (posArgs, keywordArgs, localVar.datatype) match {
    case (posArgs, List(), funDatatype: FunDatatype) if funDatatype.params == posArgs.map(_.returnType) => Some((localVar, posArgs))
    case _ => None
  }
}

case class GlobalOverloadLayer(globalVars: List[GlobalVar]) extends OverloadLayer {
  override def apply(funRange: FilePosRange, posArgs: List[AnalyzedExpr], keywordArgs: List[(String, AnalyzedExpr)]): Option[(AnalyzerVar, List[AnalyzedExpr])] = {
    def fit(globalVar: GlobalVar): Option[(AnalyzerVar, List[AnalyzedExpr])] = (posArgs, keywordArgs, globalVar.constVal) match {
      case (posArgs, keywordArgs, Some(ConstFunction(function))) =>
        val (noArgs, keywordAsPosArgs) = keywordArgs.partitionMap { case (str, expr) =>
          function.argNameToIndex.get(str) match {
            case Some(index) => Right((expr, index))
            case None => Left(expr)
          }
        }
        if (noArgs.nonEmpty) return None
        val (multipleArgs, args) = (posArgs.zipWithIndex ::: keywordAsPosArgs).groupMap(_._2)(_._1).partitionMap {
          case (index, List(arg)) => Right((index, arg))
          case (index, args) => Left((index, args))
        }
        if (multipleArgs.nonEmpty) throw Error.todo(funRange.file)
        val argsMap = args.map(_._1).toSet
        if (Range(0, function.argTypes.length).forall(argsMap.contains)) {
          Some(args.toList.sortWith((t1, t2) => t1._1 > t2._1).map(_._2)).filter(function.argTypes == _.map(_.returnType)).map((globalVar, _))
        } else
          None
      case (posArgs, List(), _) => globalVar.datatype match {
        case funDatatype: FunDatatype if funDatatype.params == posArgs.map(_.returnType) => Some((globalVar, posArgs))
        case _ => None
      }
      case _ => None
    }

    globalVars.map(fit).filter(_.nonEmpty).map(_.get) match {
      case List() => None
      case List(single) => Some(single)
      case varList => throw Error(Error.SEMANTIC, funRange.file, ErrorComponent(funRange, Some("Multiple matches for overloaded reference")) :: varList.map { case (globalVar, _) => ErrorComponent(globalVar.range, Some("One match")) }, Some("Ambiguous reference"))
    }
  }
}

class ExprParsingContext(val module: Module, val fun: Option[UserFun]) {
  private var vars = List[LocalVar]()

  def lookup(name: String, range: FilePosRange): Option[AnalyzerVar] = {
    @tailrec
    def rec(list: List[AnalyzerVar]): Option[AnalyzerVar] = if (list.isEmpty) {
      fun.flatMap(_.params.get(name)).orElse(module.vars.get(name).map {
        case List(globalVar) => globalVar
        case varList => throw Error(Error.SEMANTIC, range.file, ErrorComponent(range, Some("Multiple matches for reference")) :: varList.map(globalVar => ErrorComponent(globalVar.range, Some("One match"))), Some("Ambiguous reference"))
      })
    } else if (list.head.name == name) {
      Some(list.head)
    } else {
      rec(list.tail)
    }

    rec(vars)
  }

  def lookupOverload(name: String): List[OverloadLayer] = {
    def rec(list: List[LocalVar]): List[OverloadLayer] = if (list.isEmpty) {
      fun.flatMap(_.params.get(name)).map(localVar => LocalOverloadLayer(localVar)).toList ::: module.vars.get(name).map(globalVars => GlobalOverloadLayer(globalVars)).toList
    } else if (list.head.name == name) {
      LocalOverloadLayer(list.head) :: rec(list.tail)
    } else {
      rec(list.tail)
    }

    rec(vars)
  }

  def add(localVar: LocalVar): LocalVar = {
    vars = localVar :: vars
    localVar
  }

  def add(localVars: List[LocalVar]): Unit = localVars.foreach(add)

  def scope[T](f: => T): (T, List[LocalVar]) = {
    val length = vars.length
    val value = f
    val (newVars, varsToDrop) = vars.splitAt(length)
    vars = newVars
    (value, varsToDrop)
  }

  override def toString: String = vars.map(_.name).mkString(", ")
}

class ExprConstEvalContext {
  private var vars = mutable.Map[LocalVar, ConstVal]()

  def lookup(localVar: LocalVar): ConstVal = {
    if (!vars.contains(localVar)) throw Error.todo(s"Failed compile time code execution: local variable not initialized", localVar.range)
    vars(localVar)
  }

  def add(localVar: LocalVar, value: ConstVal): ConstVal = {
    vars(localVar) = value
    value
  }

  def add(pattern: AnalyzedPattern[LocalVar], value: ConstVal): ConstVal = {
    pattern match {
      case AnalyzedVarPattern(patternVar, _) => add(patternVar, value)
      case AnalyzedTuplePattern(elements, _) => elements.zip(value.asInstanceOf[ConstTuple].elements).foreach(t => add(t._1, t._2))
    }
    value
  }

  def scope[T](f: => T): T = {
    val copy = vars.clone()
    val value = f
    vars = copy
    value
  }
}

class ExprCodeGenContext {
  private var varOffsets = mutable.Map[LocalVar, Int]()
  private val instructions = mutable.Buffer[Instr]()

  var offset = 0
  var secondaryOffset = 0

  def lookup(localVar: LocalVar): Int = varOffsets(localVar)

  def add(localVar: LocalVar): Int = {
    offset = offset.roundDown(localVar.datatype.align) - localVar.datatype.size
    varOffsets(localVar) = offset
    offset
  }

  def add(pattern: AnalyzedPattern[LocalVar]): Unit = pattern match {
    case AnalyzedVarPattern(patternVar, _) => add(patternVar)
    case AnalyzedTuplePattern(elements, _) => elements.foreach(add)
  }

  def add(instr: Instr): Unit = if instr.redundant then () else instructions.append(instr)

  def add(instr: Instr*): Unit = instr.foreach(add)

  def scope[T](f: => T): T = {
    val copy = varOffsets.clone()
    val copyOffset = offset
    val value = f
    varOffsets = copy
    offset = copyOffset
    value
  }

  def code: List[Instr] = instructions.toList
}

def analyzeExpr(ctx: ExprParsingContext)(expr: Expr): AnalyzedExpr = expr match {
  case CallExpr(RefExpr(iden, funRange), posArgs, keywordArgs, range) =>
    val overloadLayers = ctx.lookupOverload(iden)
    val analyzedPosArgs = posArgs.map(analyzeExpr(ctx))
    val analyzedKeywordArgs = keywordArgs.map { case (name, argExpr) => (name, analyzeExpr(ctx)(argExpr)) }
    val appliedLayers = overloadLayers.map(_.apply(funRange, analyzedPosArgs, analyzedKeywordArgs))
    appliedLayers.find(_.nonEmpty).flatten match {
      case Some((globalVar: GlobalVar, args)) => AnalyzedCallExpr(AnalyzedGlobalVarRefExpr(globalVar, funRange), args, range)
      case Some((localVar: LocalVar, args)) => AnalyzedCallExpr(AnalyzedLocalVarRefExpr(localVar, funRange), args, range)
      case _ => throw Error.todo(funRange)
    }
  case CallExpr(function, posArgs, List(), range) =>
    val analyzedFunExpr = analyzeExpr(ctx)(function)
    analyzedFunExpr.returnType match {
      case FunDatatype(params, _) =>
        if (params.length != posArgs.length) throw Error.todo(range.file)
        val analyzedArgs = posArgs.map(analyzeExpr(ctx))
        if (!params.zip(analyzedArgs.map(_.returnType)).forall(t => t._1 == t._2)) throw Error.todo(range)
        AnalyzedCallExpr(analyzedFunExpr, analyzedArgs, range)
      case datatype => throw Error.semantic(s"'$datatype' is not callable", function.range)
    }
  case CallExpr(function, _, keywordArgs, range) => throw Error(Error.SEMANTIC, range.file,
    ErrorComponent(function.range, Some("This is an expression that, when evaluated, returns a function\nIn order to use keyword arguments, a direct reference is required")) :: keywordArgs.map { case (_, argExpr) => ErrorComponent(argExpr.range, Some("Disallowed keyword argument")) },
    Some("Keyword arguments are not allowed for indirect function calls")
  )
  case RefExpr(iden, range) => ctx.lookup(iden, range) match {
    case Some(globalVar: GlobalVar) => AnalyzedGlobalVarRefExpr(globalVar, range)
    case Some(localVar: LocalVar) => AnalyzedLocalVarRefExpr(localVar, range)
    case None => throw Error.todo(ctx.toString, range)
  }
  case IntExpr(int, range) => AnalyzedIntExpr(int, range)
  case TupleExpr(elements, range) => AnalyzedTupleExpr(elements.map(analyzeExpr(ctx)), range)
  case BlockExpr(exprs, lastExpr, range) => {
    val ((analyzedExprs, analyzedLastExpr), vars) = ctx.scope((exprs.map(analyzeExpr(ctx)), analyzeExpr(ctx)(lastExpr)))
    AnalyzedBlockExpr(analyzedExprs, analyzedLastExpr, vars, range)
  }
  case UnitExpr(range) => AnalyzedUnitExpr(range)
  case LetExpr(pattern, expr, range) =>
    lazy val analyzedExpr = analyzeExpr(ctx)(expr)
    lazy val (analyzedPattern: AnalyzedPattern[LocalVar], vars: List[LocalVar]) = AnalyzedPattern.map((pattern, patternNav) => new LocalVar(ctx.module, pattern.name, Some(letExpr), patternNav, pattern.datatype, pattern.range), pattern)
    if (analyzedPattern.datatype != analyzedExpr.returnType) throw Error.typeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
    lazy val letExpr = AnalyzedLetExpr(analyzedPattern, analyzedExpr, range)
    ctx.add(vars)
    letExpr
  case FunExpr(parameters, returnType, expr, range) => AnalyzedFunExpr(new UserFun(ctx.module, "", parameters, returnType, expr, range), range)
  case FunTypeExpr(parameters, returnType, range) => AnalyzedFunTypeExpr(parameters.map(analyzeExpr(ctx)), analyzeExpr(ctx)(returnType), range)
}
