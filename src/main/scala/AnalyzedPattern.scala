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

def mapPattern[T <: AnalyzerVar](varConstructor: (String, ConstVal => ConstVal, Expr) => T, pattern: Pattern): AnalyzedPattern[T] = {
  def tupleNav(index: Int)(constVal: ConstVal): ConstVal = constVal.asInstanceOf[ConstTuple].elements(index)

  def rec(pattern: Pattern, patternNav: Option[ConstVal => ConstVal]): AnalyzedPattern[T] = pattern match {
    case VarPattern(name, datatype, range) =>
      val variable = varConstructor(name, patternNav.getOrElse(constVal => constVal), datatype)
      AnalyzedVarPattern[T](variable, range)
    case TuplePattern(elements, range) =>
      AnalyzedTuplePattern[T](elements.zipWithIndex.map((pattern, index) => rec(pattern, Some(patternNav match {
        case Some(f) => f andThen tupleNav(index)
        case None => tupleNav(index)
      }))), range)
  }

  rec(pattern, None)
}
