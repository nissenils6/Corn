import scala.collection.mutable

trait AnalyzerVar {
  def name: String

  def datatype: Datatype
}

trait GlobalVar extends AnalyzerVar {
  def module: Module

  def constVal: Option[ConstVal]
}

class BuiltinGlobalVar(val module: Module, val name: String, value: ConstVal) extends GlobalVar {
  override def datatype: Datatype = value.datatype

  override def constVal: Option[ConstVal] = Some(value)

  def format(indentation: Int): String = s"builtin $name = $value"
}

class UserGlobalVar(val module: Module, val name: String, init: => UserGlobalVarInit, patternNav: ConstVal => ConstVal, typeExpr: Expr) extends GlobalVar {
  lazy val datatype: Datatype = analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype
  lazy val constVal: Option[ConstVal] = init.analyzedExpr.constVal.map(patternNav)
}

class UserGlobalVarInit(val module: Module, expr: Expr, pattern: => AnalyzedPattern[UserGlobalVar]) {
  lazy val analyzedExpr: AnalyzedExpr = analyzeExpr(ExprParsingContext(module, None))(expr)
  lazy val analyzedPattern: AnalyzedPattern[UserGlobalVar] = pattern
  
  def typeCheck(): Unit = {
    if(analyzedPattern.datatype != analyzedExpr.returnType) throw Error.unimplemented(module.file)
  }

  def format(indentation: Int): String = s"let ${pattern.format(indentation)} = ${analyzedExpr.format(indentation)}"
}

class GlobalFun(val module: Module, val name: String, retTypeExpr: Expr, expr: Expr) {
  val params: mutable.Map[String, LocalVar] = mutable.Map()
  var args: List[AnalyzedPattern[LocalVar]] = null
  lazy val returnType: Datatype = analyzeExpr(ExprParsingContext(module, None))(retTypeExpr).constDatatype
  lazy val analyzedExpr: AnalyzedExpr = analyzeExpr(ExprParsingContext(module, Some(this)))(expr)

  def typeCheck(): Unit = {
    if (returnType != analyzedExpr.returnType) throw Error.unimplemented(module.file)
  }
  
  def format(indentation: Int): String =
    s"fun $name(${args.map(_.format(indentation)).mkString}): $returnType => ${analyzedExpr.format(indentation)}"
}

class GlobalFunTable(val module: Module, val name: String) {
  val funs = mutable.Buffer[GlobalFun]()

  def format(indentation: Int): String = s"${funs.map(f => s"${" " * indentation}${f.format(indentation)}\n").mkString}"
}

class Module(val file: File) {
  val vars = mutable.Map[String, GlobalVar]()
  val varInits = mutable.Buffer[UserGlobalVarInit]()
  val funTables = mutable.Map[String, GlobalFunTable]()

  def format(indentation: Int): String = {
    val formattedBuiltin = vars.filter(_._2.isInstanceOf[BuiltinGlobalVar]).map(v => s"${v.asInstanceOf[BuiltinGlobalVar].format(indentation + 1)}\n").mkString
    val builtinNewline = if formattedBuiltin.nonEmpty then "\n" else ""
    val formattedVars = varInits.map(v => s"${" " * (indentation + 1)}${v.format(indentation + 1)}\n").mkString
    val varNewline = if formattedVars.nonEmpty then "\n" else ""
    val formattedFuns = funTables.map(_._2.format(indentation + 1)).mkString("\n")
    val funNewline = if formattedFuns.nonEmpty then "\n" else ""
    s"${" " * indentation}module ${file.name} {$builtinNewline$formattedBuiltin$varNewline$formattedVars$funNewline$formattedFuns${" " * indentation}}"
  }
}
