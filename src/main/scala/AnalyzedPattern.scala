abstract class AnalyzedPattern[T <: AnalyzerVar] {
  def range: FilePosRange

  lazy val datatype: Datatype = this match {
    case AnalyzedVarPattern(patternVar, _) => patternVar.datatype
    case AnalyzedTuplePattern(elements, _) => TupleDatatype(elements.map(_.datatype))
  }

  def format(indentation: Int): String = this match {
    case AnalyzedVarPattern(patternVar, _) => s"${patternVar.name}: $datatype"
    case AnalyzedTuplePattern(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
  }
}

case class AnalyzedVarPattern[T <: AnalyzerVar](patternVar: T, range: FilePosRange) extends AnalyzedPattern[T]

case class AnalyzedTuplePattern[T <: AnalyzerVar](elements: List[AnalyzedPattern[T]], range: FilePosRange) extends AnalyzedPattern[T]

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

object AnalyzedPattern {

  def map[T <: AnalyzerVar](varConstructor: (VarPattern, PatternNav) => T, pattern: Pattern): (AnalyzedPattern[T], List[T]) = {
    def rec(pattern: Pattern, patternNav: PatternNav): (AnalyzedPattern[T], List[T]) = pattern match {
      case varPattern: VarPattern =>
        val variable = varConstructor(varPattern, patternNav)
        (AnalyzedVarPattern[T](variable, varPattern.range), List(variable))
      case TuplePattern(elements, range) =>
        val (analyzedPatterns, vars) = elements.zipWithIndex.map((pattern, index) => rec(pattern, TuplePatternNav(index, patternNav))).unzip
        (AnalyzedTuplePattern[T](analyzedPatterns, range), vars.flatten)
    }

    rec(pattern, VarPatternNav)
  }

  def verify[T <: AnalyzerVar](vars: List[T]): Map[String, T] = {
    val (errors, verifiedVars) = vars.groupBy(_.name).partitionMap {
      case (name, List(variable)) => Right((name, variable))
      case (name, varList) => Left(Error(Error.SEMANTIC, varList.head.range.file, varList.map(variable => ErrorComponent(variable.range)), Some(s"Duplicate variables named '$name'")))
    }
    if (errors.nonEmpty) throw ErrorGroup(errors.toList)
    verifiedVars.toMap
  }

}
