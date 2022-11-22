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
}

case class VarPattern[T <: Var](patternVar: T, range: FilePosRange) extends Pattern[T]

case class TuplePattern[T <: Var](elements: List[Pattern[T]], range: FilePosRange) extends Pattern[T]

object Pattern {
  def generateIr(pattern: Pattern[LocalVar], expr: opt.Dataflow, nextCtrl: opt.Controlflow, localVars: Map[LocalVar, Int]): opt.Controlflow = pattern match {
    case VarPattern(patternVar, _) =>
      lazy val writeOp = opt.WriteLocal(localVars(patternVar), expr, nextCtrl)
      lazy val writeCtrl = opt.Controlflow(() => writeOp)
      writeCtrl
    case TuplePattern(elements, _) =>
      lazy val tupleCtrl = elements.zipWithIndex.foldRight(nextCtrl)((tuple, ctrl) => {
        val (subPattern, index) = tuple
        lazy val idxOp = opt.TupleIdx(expr, index, patternCtrl)
        lazy val patternCtrl = generateIr(subPattern, idxData, ctrl, localVars)
        lazy val idxCtrl = opt.Controlflow(() => idxOp)
        lazy val idxData = opt.Dataflow(() => Some(idxOp))
        idxCtrl
      })
  }
}

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
  def map[T <: Var](varConstructor: (syn.VarPattern, PatternNav) => T, pattern: syn.Pattern): (Pattern[T], List[T]) = {
    def rec(pattern: syn.Pattern, patternNav: PatternNav): (Pattern[T], List[T]) = pattern match {
      case varPattern: syn.VarPattern =>
        val variable = varConstructor(varPattern, patternNav)
        (VarPattern[T](variable, varPattern.range), List(variable))
      case syn.TuplePattern(elements, range) =>
        val (analyzedPatterns, vars) = elements.zipWithIndex.map((pattern, index) => rec(pattern, TuplePatternNav(index, patternNav))).unzip
        (TuplePattern[T](analyzedPatterns, range), vars.flatten)
    }

    rec(pattern, VarPatternNav)
  }

  def verify[T <: Var](vars: List[T]): Map[String, T] = {
    val (errors, verifiedVars) = vars.groupBy(_.name).partitionMap {
      case (name, List(variable)) => Right((name, variable))
      case (name, varList) => Left(Error(Error.SEMANTIC, varList.head.range.file, varList.map(variable => ErrorComponent(variable.range)), Some(s"Duplicate variables named '$name'")))
    }
    if (errors.nonEmpty) throw ErrorGroup(errors.toList)
    verifiedVars.toMap
  }
}
