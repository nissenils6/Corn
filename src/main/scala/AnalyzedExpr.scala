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
    case AnalyzedBlockExpr(_, lastExpr, _) => lastExpr.returnType
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
    case AnalyzedBlockExpr(_, _, _) => constEval(ExprConstEvalContext())
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
    case AnalyzedIntExpr(int, range) => throw Error.datatypeExpected(IntDatatype, range)
    case AnalyzedTupleExpr(elements, _) => TupleDatatype(elements.map(_.constDatatype))
    case AnalyzedBlockExpr(_, _, range) => mapToConstDatatype(constEval(ExprConstEvalContext()), range)
    case AnalyzedUnitExpr(_) => UnitDatatype
    case AnalyzedLetExpr(_, expr, range) => mapToConstDatatype(expr.constVal, range)
  }

  def constEval(ctx: ExprConstEvalContext): Option[ConstVal] = this match {
    case AnalyzedCallExpr(function, args, _) => (for {
      fun <- function.constEval(ctx)
      argValues <- args.map(_.constEval(ctx)).extract
    } yield fun.asInstanceOf[ConstFunction].function.constEval(argValues)).flatten
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.constVal
    case AnalyzedLocalVarRefExpr(localVar, _) => Some(ctx(localVar))
    case AnalyzedGlobalFunRefExpr(globalFun, _) => Some(ConstFunction(globalFun))
    case AnalyzedIntExpr(int, _) => Some(ConstInt(int))
    case AnalyzedTupleExpr(elements, _) => elements.map(element => element.constEval(ctx)).extract.map(vals => ConstTuple(vals))
    case AnalyzedBlockExpr(exprs, lastExpr, _) => ctx.scope({
      exprs.foreach(_.constEval(ctx))
      lastExpr.constEval(ctx)
    })
    case AnalyzedUnitExpr(_) => Some(ConstUnit)
    case AnalyzedLetExpr(pattern, expr, _) => expr.constEval(ctx).map(value => {
      ctx(pattern) = value
      value
    })
  }

  def format(indentation: Int): String = this match {
    case AnalyzedCallExpr(function, args, _) => s"${function.format(indentation)}(${args.map(_.format(indentation)).mkString(", ")})"
    case AnalyzedGlobalVarRefExpr(globalVar, _) => s"${globalVar.name}"
    case AnalyzedLocalVarRefExpr(localVar, _) => s"${localVar.name}"
    case AnalyzedGlobalFunRefExpr(globalFun, _) => s"${globalFun.name}"
    case AnalyzedIntExpr(int, _) => s"$int"
    case AnalyzedTupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case AnalyzedBlockExpr(exprs, lastExpr, _) => s"{\n${exprs.map(e => s"${" " * (indentation + 1)}${e.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
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

case class AnalyzedBlockExpr(exprs: List[AnalyzedExpr], lastExpr: AnalyzedExpr, range: FilePosRange) extends AnalyzedExpr

case class AnalyzedUnitExpr(range: FilePosRange) extends AnalyzedExpr

case class AnalyzedLetExpr(pattern: AnalyzedPattern[LocalVar], expr: AnalyzedExpr, range: FilePosRange) extends AnalyzedExpr

class LocalVar(val module: Module, val name: String, typeExpr: Expr, val range: FilePosRange) extends AnalyzerVar {
  lazy val datatype: Datatype = analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype
}

case class ExprParsingContext(module: Module, fun: Option[UserGlobalFun]) {
  private var vars = List[AnalyzerVar]()

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

  def addVar(analyzerVar: AnalyzerVar): Unit = vars = analyzerVar :: vars

  def scope[T](f: => T): T = {
    val length = vars.length
    val value = f
    vars = vars.drop(vars.length - length)
    value
  }
}

case class ExprConstEvalContext() {
  private var vars = mutable.Map[LocalVar, ConstVal]()

  def update(localVar: LocalVar, value: ConstVal): Unit = vars(localVar) = value

  def update(pattern: AnalyzedPattern[LocalVar], value: ConstVal): Unit = pattern match {
    case AnalyzedVarPattern(patternVar, _) => update(patternVar, value)
    case AnalyzedTuplePattern(elements, _) => elements.zip(value.asInstanceOf[ConstTuple].elements).foreach(t => update(t._1, t._2))
  }

  def apply(localVar: LocalVar): ConstVal = {
    if (!vars.contains(localVar)) throw Error.internal(s"Failed compile time code execution: local variable not initialized", localVar.range)
    vars(localVar)
  }

  def scope[T](f: => T): T = {
    val copy = vars.clone()
    val value = f
    vars = copy
    value
  }
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
  case BlockExpr(exprs, lastExpr, range) => AnalyzedBlockExpr(ctx.scope(exprs.map(analyzeExpr(ctx))), analyzeExpr(ctx)(lastExpr), range)
  case UnitExpr(range) => AnalyzedUnitExpr(range)
  case LetExpr(pattern, expr, range) =>
    val analyzedExpr = analyzeExpr(ctx)(expr)
    val analyzedPattern = mapPattern((name, patternNav, typeExpr, range) => {
      val localVar = new LocalVar(ctx.module, name, typeExpr, range)
      ctx.addVar(localVar)
      localVar
    }, pattern)
    if (analyzedPattern.datatype != analyzedExpr.returnType) throw Error.assignTypeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
    AnalyzedLetExpr(analyzedPattern, analyzedExpr, range)

}
