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
}

case object VarPatternNav extends PatternNav

case class TuplePatternNav(index: Int, next: PatternNav) extends PatternNav

def mapPattern[T <: AnalyzerVar](varConstructor: (VarPattern, PatternNav) => T, pattern: Pattern): (AnalyzedPattern[T], List[T]) = {
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

def mapPatterns[T <: AnalyzerVar](varConstructor: (VarPattern, PatternNav) => T, patterns: List[Pattern]): (List[AnalyzedPattern[T]], List[T]) = {
  val (analyzedPatterns, vars) = patterns.map(p => mapPattern(varConstructor, p)).unzip
  (analyzedPatterns, vars.flatten)
}

def verifyPatterns[T <: AnalyzerVar](vars: List[T]): Map[String, T] = {
  vars.groupBy(_.name).map{case (name, vars) => (name, vars.head)}
}

def mapAndVerify[T <: AnalyzerVar](varConstructor: (VarPattern, PatternNav) => T, patterns: List[Pattern]): (List[AnalyzedPattern[T]], Map[String, T]) = {
  val (a, b) = mapPatterns(varConstructor, patterns)
  (a, verifyPatterns(b))
}
