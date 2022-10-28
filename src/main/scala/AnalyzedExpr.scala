import scala.annotation.tailrec
import scala.collection.immutable.List

abstract class AnalyzedExpr() {
  def range: FilePosRange

  lazy val returnType: Datatype = this match {
    case AnalyzedCallExpr(function, _, _) => function.returnType.asInstanceOf[FunDatatype].returnType
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.datatype
    case AnalyzedLocalVarRefExpr(localVar, _) => localVar.datatype
    case AnalyzedGlobalFunRefExpr(globalFun, _) => FunDatatype(globalFun.args.map(_.datatype), globalFun.returnType)
    case AnalyzedIntExpr(_, _) => IntDatatype
    case AnalyzedTupleExpr(elements, _) => TupleDatatype(elements.map(_.returnType))
    case AnalyzedBlockExpr(_, lastExpr, _) => lastExpr.returnType
    case AnalyzedUnitExpr(_) => UnitDatatype
    case AnalyzedLetExpr(_, expr, _) => expr.returnType
  }

  lazy val constVal: Option[ConstVal] = this match {
    case AnalyzedCallExpr(function, args, _) => None
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.constVal
    case AnalyzedLocalVarRefExpr(localVar, _) => None
    case AnalyzedGlobalFunRefExpr(globalFun, _) => Some(ConstFunction(globalFun))
    case AnalyzedIntExpr(int, _) => Some(ConstInt(int))
    case AnalyzedTupleExpr(elements, _) =>
      val data = elements.map(_.constVal)
      if (data.forall(_.nonEmpty)) {
        Some(ConstTuple(data.map(_.get)))
      } else {
        None
      }
    case AnalyzedBlockExpr(exprs, lastExpr, _) => None
    case AnalyzedUnitExpr(_) => Some(ConstUnit)
    case AnalyzedLetExpr(pattern, expr, _) => None
  }

  lazy val constDatatype: Datatype = this match {
    case AnalyzedCallExpr(function, args, _) => throw Error.unimplemented(range.file)
    case AnalyzedGlobalVarRefExpr(globalVar, _) => globalVar.constVal match {
      case Some(ConstType(datatype)) => datatype
      case None => throw Error.unimplemented(range.file)
    }
    case AnalyzedLocalVarRefExpr(localVar, _) => throw Error.unimplemented(range.file)
    case AnalyzedGlobalFunRefExpr(globalFun, _) => throw Error.unimplemented(range.file)
    case AnalyzedIntExpr(int, _) => throw Error.unimplemented(range.file)
    case AnalyzedTupleExpr(elements, _) => TupleDatatype(elements.map(_.constDatatype))
    case AnalyzedBlockExpr(exprs, lastExpr, _) => throw Error.unimplemented(range.file)
    case AnalyzedUnitExpr(_) => UnitDatatype
    case AnalyzedLetExpr(pattern, expr, _) => throw Error.unimplemented(range.file)
  }

  def format(indentation: Int): String = this match {
    case AnalyzedCallExpr(function, args, _) => s"${function.format(indentation)}(${args.map(_.format(indentation)).mkString})"
    case AnalyzedGlobalVarRefExpr(globalVar, _) => s"${globalVar.name}"
    case AnalyzedLocalVarRefExpr(localVar, _) => s"${localVar.name}"
    case AnalyzedGlobalFunRefExpr(globalFun, _) => s"${globalFun.name}"
    case AnalyzedIntExpr(int, _) => s"$int"
    case AnalyzedTupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case AnalyzedBlockExpr(exprs, lastExpr, _) =>
      s"{\n${exprs.map(e => s"${" " * (indentation + 1)}${e.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
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

class LocalVar(val module: Module, val name: String, typeExpr: Expr) extends AnalyzerVar {
  lazy val datatype: Datatype = analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype
}

case class ExprParsingContext(module: Module, fun: Option[GlobalFun]) {
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

private def iterateLocalPattern(ctx: ExprParsingContext)(pattern: Pattern): AnalyzedPattern[LocalVar] = pattern match {
  case VarPattern(name, datatype, range) =>
    val localVar = new LocalVar(ctx.module, name, datatype)
    ctx.addVar(localVar)
    AnalyzedVarPattern[LocalVar](localVar, range)
  case TuplePattern(elements, range) =>
    AnalyzedTuplePattern(elements.map(iterateLocalPattern(ctx)), range)
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
          throw Error.unimplemented(range.file)
        }
        AnalyzedCallExpr(analyzedFunExpr, analyzedArgs, range)
      case _ => throw Error.unimplemented(range.file)
    }
  case RefExpr(iden, range) => ctx.lookup(iden) match {
    case Some(v) => v match {
      case Right(globalFunTable: GlobalFunTable) => AnalyzedGlobalFunRefExpr(globalFunTable.funs.head, range)
      case Left(globalVar: GlobalVar) => AnalyzedGlobalVarRefExpr(globalVar, range)
      case Left(localVar: LocalVar) => AnalyzedLocalVarRefExpr(localVar, range)
    }
  }
  case IntExpr(int, range) => AnalyzedIntExpr(int, range)
  case TupleExpr(elements, range) => AnalyzedTupleExpr(elements.map(analyzeExpr(ctx)), range)
  case BlockExpr(exprs, lastExpr, range) => AnalyzedBlockExpr(ctx.scope(exprs.map(analyzeExpr(ctx))), analyzeExpr(ctx)(lastExpr), range)
  case UnitExpr(range) => AnalyzedUnitExpr(range)
  case LetExpr(pattern, expr, range) =>
    val analyzedPattern = mapPattern((name, patternNav, typeExpr) => {
      val localVar = new LocalVar(ctx.module, name, typeExpr)
      ctx.addVar(localVar)
      localVar
    }, pattern)
    val analyzedExpr = analyzeExpr(ctx)(expr)
    if(analyzedPattern.datatype != analyzedExpr.returnType) throw Error.unimplemented(ctx.module.file)
    AnalyzedLetExpr(analyzedPattern, analyzedExpr, range)

}
