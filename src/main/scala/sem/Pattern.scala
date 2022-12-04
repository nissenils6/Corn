package sem

import core.*
import gen.*

abstract class Pattern[T <: Var] {
  def range: FilePosRange

  lazy val datatype: Datatype = this match {
    case VarPattern(patternVar, _) => patternVar.datatype
    case TuplePattern(elements, _) => TupleDatatype(elements.map(_.datatype), false)
  }

  def format(indentation: Int): String = this match {
    case VarPattern(patternVar, _) => s"${patternVar.name}: $datatype"
    case TuplePattern(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
  }

  def gatherVars: List[T] = this match {
    case VarPattern(patternVar, _) => List(patternVar)
    case TuplePattern(elements, _) => elements.flatMap(_.gatherVars)
  }
}

case class VarPattern[T <: Var](patternVar: T, range: FilePosRange) extends Pattern[T]
case class TuplePattern[T <: Var](elements: List[Pattern[T]], range: FilePosRange) extends Pattern[T]

abstract class PatternNav {
  def const(constVal: ConstVal): ConstVal = this match {
    case VarPatternNav => constVal
    case TuplePatternNav(index, next) => next.const(constVal.asInstanceOf[ConstTuple].elements(index))
  }

  def datatype(returnType: Datatype): Datatype = this match {
    case VarPatternNav => returnType
    case TuplePatternNav(index, next) => next.datatype(returnType.asInstanceOf[TupleDatatype].elements(index))
  }
}

case object VarPatternNav extends PatternNav
case class TuplePatternNav(index: Int, next: PatternNav) extends PatternNav

object Pattern {
  def analyze[T <: Var](varConstructor: (syn.VarPattern, PatternNav) => T, pattern: syn.Pattern): (Pattern[T], List[T]) = {
    def rec(pattern: syn.Pattern, patternNav: PatternNav): (Pattern[T], List[T]) = pattern match {
      case varPattern: syn.VarPattern =>
        val variable = varConstructor(varPattern, patternNav)
        (VarPattern[T](variable, varPattern.range), List(variable))
      case syn.TuplePattern(elements, range) =>
        val (analyzedPatterns: List[Pattern[T]], vars: List[List[T]]) = elements.zipWithIndex.map((pattern, index) => rec(pattern, TuplePatternNav(index, patternNav))).unzip
        (TuplePattern[T](analyzedPatterns, range), vars.flatten)
    }

    rec(pattern, VarPatternNav)
  }

  def verify[T <: Var](vars: List[T]): Map[String, T] = {
    val (errors: List[Error], verifiedVars: List[(String, T)]) = vars.groupBy(_.name).partitionMap {
      case (name, List(variable)) => Right((name, variable))
      case (name, varList) => Left(Error(Error.SEMANTIC, varList.head.range.file, varList.map(variable => ErrorComponent(variable.range)), Some(s"Duplicate variables named '$name'")))
    }
    if (errors.nonEmpty) throw ErrorGroup(errors.toList)
    verifiedVars.toMap
  }

  def generateIrLocal(pattern: Pattern[LocalVar], expr: opt.Data, localVars: Map[LocalVar, Int]): List[opt.Op] = pattern match {
    case VarPattern(patternVar, _) => List(opt.WriteLocal(localVars(patternVar), expr))
    case TuplePattern(elements, _) =>
      elements.zipWithIndex.flatMap { case (subPattern, idx) =>
        val idxOp = opt.TupleIdx(expr, idx)
        idxOp :: generateIrLocal(subPattern, opt.toData(idxOp), localVars)
      }
  }

  def generateIrGlobal(pattern: Pattern[UserGlobalVar], expr: opt.Data, context: IrGenContext): List[opt.Op] = pattern match {
    case VarPattern(patternVar, _) =>
      val (optVar: opt.Var, optVarIdx: Int) = context(patternVar)
      List(opt.WriteGlobal(optVar, optVarIdx, expr))
    case TuplePattern(elements, _) =>
      elements.zipWithIndex.flatMap { case (subPattern, idx) =>
        val idxOp = opt.TupleIdx(expr, idx)
        idxOp :: generateIrGlobal(subPattern, opt.toData(idxOp), context)
      }
  }
}
