import scala.collection.mutable

abstract class AnalyzerVar {
  def name: String
  def range: FilePosRange
  def datatype: Datatype
}

abstract class GlobalVar extends AnalyzerVar {
  def constVal: Option[ConstVal]

  lazy val label: Option[String] = if datatype.runtime then Some((constVal match {
    case Some(value) => CodeGen.data(value)
    case None => CodeGen.bss(datatype.size, datatype.align)
  })._1) else None
}

class BuiltinGlobalVar(module: => Module, val name: String, value: ConstVal) extends GlobalVar {
  override def range: FilePosRange = module.file.lastRange

  lazy val datatype: Datatype = value.datatype
  lazy val constVal: Option[ConstVal] = Some(value)

  def format(indentation: Int): String = s"${" " * indentation}builtin $name: $datatype = $value"
}

class UserGlobalVar(module: => Module, val name: String, init: => UserGlobalVarInit, patternNav: PatternNav, typeExpr: Expr, val range: FilePosRange) extends GlobalVar {
  lazy val datatype: Datatype = analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype
  lazy val constVal: Option[ConstVal] = init.analyzedExpr.constVal.map(patternNav.const)
}

class UserGlobalVarInit(module: => Module, expr: Expr, pattern: => AnalyzedPattern[UserGlobalVar]) {
  lazy val analyzedExpr: AnalyzedExpr = analyzeExpr(ExprParsingContext(module, None))(expr)
  lazy val analyzedPattern: AnalyzedPattern[UserGlobalVar] = pattern

  def typeCheck(): Unit = {
    if (analyzedPattern.datatype != analyzedExpr.returnType) throw Error.assignTypeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
  }

  def format(indentation: Int): String = s"${" " * indentation}${pattern.format(indentation)} = ${analyzedExpr.format(indentation)}${analyzedExpr.constVal.map(value => s"    [CONST: ${value.toString}]").getOrElse("")}"
}

abstract class Fun {
  def range: FilePosRange
  def argTypes: List[Datatype]
  def returnType: Datatype

  def format(indentation: Int): String
  def typeCheck(): Unit

  def constEval(args: List[ConstVal]): Option[ConstVal]
  def generateCode: List[Instr]

  lazy val signature: FunDatatype = FunDatatype(argTypes, returnType)
  lazy val label: String = CodeGen.function(generateCode)
}

class BuiltinFun(val module: Module, val argTypes: List[Datatype], val returnType: Datatype, evalFunction: List[ConstVal] => Option[ConstVal], generateFunction: () => List[Instr]) extends Fun {
  override def range: FilePosRange = module.file.lastRange

  override def format(indentation: Int): String = s"builtin fun(${argTypes.mkString(", ")}): $returnType"

  override def typeCheck(): Unit = ()

  override def constEval(args: List[ConstVal]): Option[ConstVal] = evalFunction(args)

  override def generateCode: List[Instr] = generateFunction()
}

class UserFun(val module: Module, val name: String, val parameters: List[Pattern], retTypeExpr: Expr, expr: Expr, val range: FilePosRange) extends Fun {
  //  val params: mutable.Map[String, LocalVar] = mutable.Map()
  //  lazy val args: List[AnalyzedPattern[LocalVar]] = parameters.map(param => mapPattern((name, _, typeExpr, range) => {
  //    if (params.contains(name)) throw Error.duplicate("parameter", name, range, params(name).range)
  //    val localVar = new LocalVar(module, name, typeExpr, range)
  //    params(name) = localVar
  //    localVar
  //  }, param))

  lazy val (args: List[AnalyzedPattern[LocalVar]], params: Map[String, LocalVar]) = mapAndVerify((pattern, _) => new LocalVar(module, pattern.name, pattern.datatype, pattern.range), parameters)

  lazy val analyzedExpr: AnalyzedExpr = analyzeExpr(new ExprParsingContext(module, Some(this)))(expr)

  lazy val argTypes: List[Datatype] = args.map(_.datatype)
  lazy val returnType: Datatype = analyzeExpr(new ExprParsingContext(module, None))(retTypeExpr).constDatatype

  override def format(indentation: Int): String = s"fun(${args.map(_.format(indentation)).mkString(", ")}): $returnType => ${analyzedExpr.format(indentation)}"

  override def typeCheck(): Unit = if (returnType != analyzedExpr.returnType) throw Error.unimplemented(module.file)

  override def constEval(arguments: List[ConstVal]): Option[ConstVal] = {
    val ctx = new ExprConstEvalContext()
    args.zip(arguments).foreach(t => ctx.add(t._1, t._2))
    analyzedExpr.constEval(ctx)
  }

  override def generateCode: List[Instr] = {
    val ctx = new ExprCodeGenContext()

    params.values.foreach(ctx.add)

    def iterateArgs(arg: AnalyzedPattern[LocalVar], offset: Int): Unit = arg match {
      case AnalyzedVarPattern(patternVar, _) => patternVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.lookup(patternVar), Reg.RBP + offset)
      case AnalyzedTuplePattern(elements, _) => elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3.map(_ + offset)).foreach(iterateArgs.tupled)
    }

    args.accumulate(0) { case (arg, offset) =>
      iterateArgs(arg, offset)
      arg.datatype.size
    }

    analyzedExpr.generateCode(ctx)

    ctx.add(Ret())
    ctx.code
  }
}

class Module(val file: File, fileContent: => (Map[String, GlobalVar], List[AnalyzedPattern[UserGlobalVar]], List[Expr])) {
  lazy val (vars, varInits) = {
    val (vars, varInits, exprs) = fileContent
    (vars, varInits.zip(exprs).map { case (analyzedPattern, expr) => new UserGlobalVarInit(this, expr, analyzedPattern) })
    //    val groupedVars = vars.groupBy(_.name)
    //    val a = (groupedVars.filter(_._2.length > 1) match {
    //      case erroneousVars if erroneousVars.nonEmpty =>
    //        val (name, firstErrorGroup) = erroneousVars.head
    //        val List(first, second) = firstErrorGroup.take(2)
    //        throw Error.duplicate("global variable", name, first.range, second.range)
    //      case _ => groupedVars.map { case (name, vars) => (name, vars.head) }
    //    }, varInits)
    //    a
  }

  def format(indentation: Int): String = {
    def section(string: String) = string match {
      case "" => ""
      case string => "\n" + string
    }

    val formattedBuiltin = section(vars.filter(_._2.isInstanceOf[BuiltinGlobalVar]).map(v => s"${v._2.asInstanceOf[BuiltinGlobalVar].format(indentation + 1)}\n").mkString)
    val formattedVars = section(varInits.map(v => s"${v.format(indentation + 1)}\n").mkString)
    s"${" " * indentation}module ${file.name} {\n$formattedBuiltin$formattedVars}\n"
  }
}
