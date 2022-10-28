import scala.collection.mutable

abstract class AnalyzerVar {
  def name: String
  def range: FilePosRange
  def datatype: Datatype
}

abstract class GlobalVar extends AnalyzerVar {
  def module: Module
  def constVal: Option[ConstVal]
}

class BuiltinGlobalVar(val module: Module, val name: String, value: ConstVal) extends GlobalVar {
  override def range: FilePosRange = module.file.lastRange

  lazy val datatype: Datatype = value.datatype
  lazy val constVal: Option[ConstVal] = Some(value)

  def format(indentation: Int): String = s"builtin let $name: $datatype = $value;"
}

class UserGlobalVar(val module: Module, val name: String, init: => UserGlobalVarInit, patternNav: ConstVal => ConstVal, typeExpr: Expr, val range: FilePosRange) extends GlobalVar {
  lazy val datatype: Datatype = analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype
  lazy val constVal: Option[ConstVal] = init.analyzedExpr.constVal.map(patternNav)
}

class UserGlobalVarInit(val module: Module, expr: Expr, pattern: => AnalyzedPattern[UserGlobalVar]) {
  lazy val analyzedExpr: AnalyzedExpr = analyzeExpr(ExprParsingContext(module, None))(expr)
  lazy val analyzedPattern: AnalyzedPattern[UserGlobalVar] = pattern

  def typeCheck(): Unit = {
    if (analyzedPattern.datatype != analyzedExpr.returnType) throw Error.assignTypeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
  }

  def format(indentation: Int): String = s"let ${pattern.format(indentation)} = ${analyzedExpr.format(indentation)};${analyzedExpr.constVal.map(value => s"    [CONST: ${value.toString}]").getOrElse("")}"
}

abstract class GlobalFun {
  def name: String
  def range: FilePosRange
  def argTypes: List[Datatype]
  def returnType: Datatype

  def format(indentation: Int): String
  def typeCheck(): Unit

  def constEval(args: List[ConstVal]): Option[ConstVal]

  lazy val signature: Datatype = FunDatatype(argTypes, returnType)
}

class BuiltinGlobalFun(val module: Module, val name: String, val argTypes: List[Datatype], val returnType: Datatype, evalFunction: List[ConstVal] => Option[ConstVal]) extends GlobalFun {
  override def range: FilePosRange = module.file.lastRange

  override def format(indentation: Int): String = s"builtin fun $name(${argTypes.mkString(", ")}): $returnType"

  override def typeCheck(): Unit = ()

  override def constEval(args: List[ConstVal]): Option[ConstVal] = evalFunction(args)
}

class UserGlobalFun(val module: Module, val name: String, retTypeExpr: Expr, expr: Expr, val range: FilePosRange) extends GlobalFun {
  val params: mutable.Map[String, LocalVar] = mutable.Map()
  var args: List[AnalyzedPattern[LocalVar]] = null

  lazy val analyzedExpr: AnalyzedExpr = analyzeExpr(ExprParsingContext(module, Some(this)))(expr)

  lazy val argTypes: List[Datatype] = args.map(_.datatype)
  lazy val returnType: Datatype = analyzeExpr(ExprParsingContext(module, None))(retTypeExpr).constDatatype

  override def format(indentation: Int): String = s"fun $name(${args.map(_.format(indentation)).mkString(", ")}): $returnType => ${analyzedExpr.format(indentation)}"

  override def typeCheck(): Unit = {
    if (returnType != analyzedExpr.returnType) throw Error.unimplemented(module.file)
  }

  override def constEval(arguments: List[ConstVal]): Option[ConstVal] = {
    val ctx = ExprConstEvalContext()
    args.zip(arguments).foreach(t => ctx(t._1) = t._2)
    analyzedExpr.constEval(ctx)
  }
}

class GlobalFunTable(val module: Module, val name: String) {
  val funs = mutable.Buffer[GlobalFun]()

  def format(indentation: Int): String = s"${funs.map(f => s"${" " * indentation}${f.format(indentation)}\n").mkString}"
}

class Module(val file: File) {
  val vars: mutable.Map[String, GlobalVar] = mutable.Map()
  val varInits: mutable.Buffer[UserGlobalVarInit] = mutable.Buffer()
  val funTables: mutable.Map[String, GlobalFunTable] = mutable.Map()

  def addVar[T <: GlobalVar](variable: T): T = {
    if (vars.contains(variable.name)) throw Error.duplicate("global variable", variable.name, variable.range, vars(variable.name).range)
    vars(variable.name) = variable
    variable
  }

  def addFun[T <: GlobalFun](function: T): T = {
    if (!funTables.contains(function.name))
      funTables(function.name) = GlobalFunTable(this, function.name)
    funTables(function.name).funs.append(function)
    function
  }

  def format(indentation: Int): String = {
    def section(string: String) = string match {
      case "" => ""
      case string => "\n" + string
    }

    val formattedBuiltin = section(vars.filter(_._2.isInstanceOf[BuiltinGlobalVar]).map(v => s"${" " * (indentation + 1)}${v._2.asInstanceOf[BuiltinGlobalVar].format(indentation + 1)}\n").mkString)
    val formattedVars = section(varInits.map(v => s"${" " * (indentation + 1)}${v.format(indentation + 1)}\n").mkString)
    val formattedFuns = section(funTables.map(_._2.format(indentation + 1)).mkString("\n"))
    s"${" " * indentation}module ${file.name} {\n$formattedBuiltin$formattedVars$formattedFuns\n${" " * indentation}}\n"
  }
}
