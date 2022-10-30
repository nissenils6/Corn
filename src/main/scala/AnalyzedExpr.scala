import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable

abstract class AnalyzedExpr() {
  def range: FilePosRange

  lazy val returnType: Datatype = this match {
    case AnalyzedCallExpr(function, _, _) => function.returnType.asInstanceOf[FunDatatype].returnType
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.datatype
    case AnalyzedLocalVarRefExpr(localVar, _) => localVar.datatype
    case AnalyzedGlobalFunRefExpr(globalFun, _) => FunDatatype(globalFun.argTypes, globalFun.returnType)
    case AnalyzedIntExpr(_, _) => IntDatatype
    case AnalyzedTupleExpr(elements, _) => TupleDatatype(elements.map(_.returnType))
    case AnalyzedBlockExpr(_, lastExpr, _, _) => lastExpr.returnType
    case AnalyzedUnitExpr(_) => UnitDatatype
    case AnalyzedLetExpr(_, expr, _) => expr.returnType
  }

  lazy val constVal: Option[ConstVal] = this match {
    case AnalyzedCallExpr(function, args, _) => (for {
      fun <- function.constVal
      argValues <- args.map(_.constVal).extract
    } yield fun.asInstanceOf[ConstFunction].function.constEval(argValues)).flatten
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.constVal
    case AnalyzedLocalVarRefExpr(_, range) => throw Error.internal(s"Attempt at compile time evaluating a local variable reference expression outside of a expression evaluation context", range)
    case AnalyzedGlobalFunRefExpr(globalFun, _) => Some(ConstFunction(globalFun))
    case AnalyzedIntExpr(int, _) => Some(ConstInt(int))
    case AnalyzedTupleExpr(elements, _) => elements.map(_.constVal).extract.map(vals => ConstTuple(vals))
    case AnalyzedBlockExpr(_, _, _, _) => constEval(ExprConstEvalContext())
    case AnalyzedUnitExpr(_) => Some(ConstUnit)
    case AnalyzedLetExpr(_, expr, _) => expr.constVal
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
    case AnalyzedLocalVarRefExpr(_, _) => throw Error.internal(s"Attempt at compile time evaluating a local variable reference expression outside of a expression evaluation context", range)
    case AnalyzedGlobalFunRefExpr(globalFun, range) => throw Error.datatypeExpected(globalFun.signature, range)
    case AnalyzedIntExpr(_, range) => throw Error.datatypeExpected(IntDatatype, range)
    case AnalyzedTupleExpr(elements, _) => TupleDatatype(elements.map(_.constDatatype))
    case AnalyzedBlockExpr(_, _, _, range) => mapToConstDatatype(constEval(ExprConstEvalContext()), range)
    case AnalyzedUnitExpr(_) => UnitDatatype
    case AnalyzedLetExpr(_, expr, range) => mapToConstDatatype(expr.constVal, range)
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
  }

  def generateCode(ctx: ExprCodeGenContext): Unit = this match {
    case AnalyzedCallExpr(function, args, _) => {
      val signature = function.returnType.asInstanceOf[FunDatatype]

      ctx.offset = ctx.offset.roundDown(signature.returnType.align) - signature.returnType.size
      val returnValueOffset = ctx.offset

      ctx.offset = ctx.offset.roundDown(16)
      val functionPtrOffset = ctx.offset
      function.generateCode(ctx)

      ctx.offset = ctx.offset.roundDown(16)
      args.foreach(_.generateCode(ctx))
      ctx.offset = ctx.offset.roundDown(16)

      ctx.add(
        Load(Reg.RAX, Reg.RSP + (functionPtrOffset - 8))
      )
      if (signature.returnType.size > 0) {
        ctx.add(
          Lea(Reg.RBX, Reg.RSP + returnValueOffset),
          Store(Reg.RSP + (functionPtrOffset - 8), Reg.RBX)
        )
      }
      ctx.add(
        Asm.Sub(Reg.RSP, -ctx.offset),
        IndRegCall(Reg.RAX),
        Asm.Add(Reg.RSP, -ctx.offset)
      )
      ctx.offset = returnValueOffset
    }
    case AnalyzedGlobalVarRefExpr(globalVar, range) => {
      ctx.offset = ctx.offset.roundDown(globalVar.datatype.align) - globalVar.datatype.size
      globalVar.label match {
        case Some(label) => globalVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.offset, Address(label))
        case None => throw Error.unimplemented(range.file)
      }
    }
    case AnalyzedLocalVarRefExpr(localVar, _) => {
      ctx.offset = ctx.offset.roundDown(localVar.datatype.align) - localVar.datatype.size
      localVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.offset, Reg.RSP + ctx.lookup(localVar))
    }
    case AnalyzedGlobalFunRefExpr(globalFun, _) => {
      ctx.offset = ctx.offset.roundDown(8) - 8
      ctx.add(
        Lea(Reg.RAX, Address(globalFun.label)),
        Store(Reg.RSP + ctx.offset, Reg.RAX)
      )
    }
    case AnalyzedIntExpr(int, _) => {
      ctx.offset = ctx.offset.roundDown(8) - 8
      ctx.add(StoreImm(Reg.RSP + ctx.offset, int))
    }
    case AnalyzedTupleExpr(elements, _) => elements.foreach(_.generateCode(ctx))
    case AnalyzedBlockExpr(exprs, lastExpr, vars, _) => ctx.scope({
      ctx.offset = ctx.offset.roundDown(lastExpr.returnType.align)
      val varOffset = ctx.offset
      vars.foreach(ctx.add)
      exprs.foreach(expr => {
        expr.generateCode(ctx)
        ctx.offset += expr.returnType.size
      })
      lastExpr.generateCode(ctx)
      ctx.offset = varOffset - lastExpr.returnType.size
      if (vars.nonEmpty && vars.exists(_.datatype.size > 0)) {
        lastExpr.returnType.generateCopyCode(ctx, Reg.RSP + ctx.offset, Address(Reg.RSP))
      }
    })
    case AnalyzedUnitExpr(_) => ()
    case AnalyzedLetExpr(pattern, expr, _) => {
      expr.generateCode(ctx)

      def iteratePattern(pattern: AnalyzedPattern[LocalVar], offset: Int): Unit = pattern match {
        case AnalyzedVarPattern(patternVar, _) => patternVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.lookup(patternVar), Reg.RSP + offset)
        case AnalyzedTuplePattern(elements, _) => elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3).foreach(t => iteratePattern(t._1, offset + t._2))
      }

      iteratePattern(pattern, ctx.offset)
    }
  }

  def format(indentation: Int): String = this match {
    case AnalyzedCallExpr(function, args, _) => s"${function.format(indentation)}(${args.map(_.format(indentation)).mkString(", ")})"
    case AnalyzedGlobalVarRefExpr(globalVar, _) => s"${globalVar.name}"
    case AnalyzedLocalVarRefExpr(localVar, _) => s"${localVar.name}"
    case AnalyzedGlobalFunRefExpr(globalFun, _) => s"${globalFun.name}"
    case AnalyzedIntExpr(int, _) => s"$int"
    case AnalyzedTupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case AnalyzedBlockExpr(exprs, lastExpr, _, _) => s"{\n${exprs.map(e => s"${" " * (indentation + 1)}${e.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
    case AnalyzedUnitExpr(_) => s"()"
    case AnalyzedLetExpr(pattern, expr, _) => s"let ${pattern.format(indentation)} = ${expr.format(indentation)}"
  }
}

case class AnalyzedCallExpr(function: AnalyzedExpr, args: List[AnalyzedExpr], range: FilePosRange) extends AnalyzedExpr

case class AnalyzedGlobalVarRefExpr(globalVar: GlobalVar, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedLocalVarRefExpr(localVar: LocalVar, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedGlobalFunRefExpr(globalFun: GlobalFun, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedIntExpr(int: Int, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedTupleExpr(elements: List[AnalyzedExpr], range: FilePosRange) extends AnalyzedExpr

case class AnalyzedBlockExpr(exprs: List[AnalyzedExpr], lastExpr: AnalyzedExpr, vars: List[LocalVar], range: FilePosRange) extends AnalyzedExpr

case class AnalyzedUnitExpr(range: FilePosRange) extends AnalyzedExpr

case class AnalyzedLetExpr(pattern: AnalyzedPattern[LocalVar], expr: AnalyzedExpr, range: FilePosRange) extends AnalyzedExpr

class LocalVar(val module: Module, val name: String, typeExpr: Expr, val range: FilePosRange) extends AnalyzerVar {
  lazy val datatype: Datatype = analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype
}

class ExprParsingContext(val module: Module, val fun: Option[UserGlobalFun]) {
  private var vars = List[LocalVar]()

  def lookup(name: String): Option[Either[AnalyzerVar, GlobalFunTable]] = {
    @tailrec
    def rec(list: List[AnalyzerVar]): Option[Either[AnalyzerVar, GlobalFunTable]] = if (list.isEmpty) {
      fun.flatMap(_.params.get(name)).orElse(module.vars.get(name)).map(v => Left(v)).orElse(module.funTables.get(name).map(t => Right(t)))
    } else if (list.head.name == name) {
      Some(Left(list.head))
    } else {
      rec(list.tail)
    }

    rec(vars)
  }

  def add(analyzerVar: LocalVar): LocalVar = {
    vars = analyzerVar :: vars
    analyzerVar
  }

  def scope[T](f: => T): (T, List[LocalVar]) = {
    val length = vars.length
    val value = f
    val (newVars, varsToDrop) = vars.splitAt(length)
    vars = newVars
    (value, varsToDrop)
  }
}

class ExprConstEvalContext {
  private var vars = mutable.Map[LocalVar, ConstVal]()

  def lookup(localVar: LocalVar): ConstVal = {
    if (!vars.contains(localVar)) throw Error.internal(s"Failed compile time code execution: local variable not initialized", localVar.range)
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

  def add(instr: Instr): Unit = instructions.append(instr)

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
  case CallExpr(function, posArgs, keywordArgs, range) =>
    val analyzedFunExpr = analyzeExpr(ctx)(function)
    analyzedFunExpr.returnType match {
      case FunDatatype(params, returnType) =>
        if (params.length != posArgs.length) {
          throw Error.unimplemented(range.file)
        }
        val analyzedArgs = posArgs.map(analyzeExpr(ctx))
        if (!params.zip(analyzedArgs.map(_.returnType)).forall(t => t._1 == t._2)) {
          throw Error.internal("", range)
        }
        AnalyzedCallExpr(analyzedFunExpr, analyzedArgs, range)
      case datatype => throw Error.semantic(s"Datatype '$datatype' is not callable", function.range)
    }
  case RefExpr(iden, range) => ctx.lookup(iden) match {
    case Some(v) => v match {
      case Right(globalFunTable: GlobalFunTable) => AnalyzedGlobalFunRefExpr(globalFunTable.funs.head, range)
      case Left(globalVar: GlobalVar) => AnalyzedGlobalVarRefExpr(globalVar, range)
      case Left(localVar: LocalVar) => AnalyzedLocalVarRefExpr(localVar, range)
    }
    case None => throw Error.internal("", range)
  }
  case IntExpr(int, range) => AnalyzedIntExpr(int, range)
  case TupleExpr(elements, range) => AnalyzedTupleExpr(elements.map(analyzeExpr(ctx)), range)
  case BlockExpr(exprs, lastExpr, range) => {
    val (analyzedExprs, vars) = ctx.scope(exprs.map(analyzeExpr(ctx)))
    AnalyzedBlockExpr(analyzedExprs, analyzeExpr(ctx)(lastExpr), vars, range)
  }
  case UnitExpr(range) => AnalyzedUnitExpr(range)
  case LetExpr(pattern, expr, range) =>
    val analyzedExpr = analyzeExpr(ctx)(expr)
    val analyzedPattern = mapPattern((name, patternNav, typeExpr, range) => {
      val localVar = new LocalVar(ctx.module, name, typeExpr, range)
      ctx.add(localVar)
      localVar
    }, pattern)
    if (analyzedPattern.datatype != analyzedExpr.returnType) throw Error.assignTypeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
    AnalyzedLetExpr(analyzedPattern, analyzedExpr, range)

}
