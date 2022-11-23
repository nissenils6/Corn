package sem

import core.*
import gen.*

import scala.collection.mutable

abstract class Var {
  def name: String

  def range: FilePosRange

  def datatype: Datatype

  def constVal: Option[ConstVal]
}

abstract class GlobalVar extends Var {
  def constVal: Option[ConstVal]

  def runtime: Boolean

  def compiletime: Boolean

  lazy val label: Option[String] = if datatype.runtime then Some(AsmGen.bss(datatype.size.roundUp(8), datatype.align)) else None
}

class BuiltinGlobalVar(module: => Module, val name: String, value: ConstVal) extends GlobalVar {
  override def range: FilePosRange = module.file.lastRange

  lazy val datatype: Datatype = value.datatype
  lazy val constVal: Option[ConstVal] = Some(value)

  lazy val runtime: Boolean = value.datatype.runtime

  lazy val compiletime: Boolean = true

  def format(indentation: Int): String = s"${" " * indentation}builtin $name: $datatype = $value"
}

class UserGlobalVar(module: => Module, val name: String, init: => UserGlobalVarInit, patternNav: PatternNav, typeExpr: Option[syn.Expr], val range: FilePosRange) extends GlobalVar {
  lazy val datatype: Datatype = typeExpr.map(typeExpr => analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype).getOrElse(patternNav.datatype(init.analyzedExpr.returnType).withMut(false))
  lazy val constVal: Option[ConstVal] = if datatype.mutable then None else init.analyzedExpr.constVal.map(patternNav.const)

  lazy val runtime: Boolean = datatype.runtime && init.analyzedExpr.runtime

  lazy val compiletime: Boolean = init.analyzedExpr.compiletime

  def initt = init
}

class UserGlobalVarInit(module: => Module, expr: syn.Expr, pattern: => Pattern[UserGlobalVar]) {
  lazy val analyzedExpr: Expr = analyzeExpr(ExprParsingContext(module, None))(expr)
  lazy val analyzedPattern: Pattern[UserGlobalVar] = pattern

  def generateIr(globalVars: Map[GlobalVar, Int], funs: Map[Fun, Int]): opt.Var = {
    lazy val (exprData: opt.Dataflow, exprCtrl: opt.Controlflow) = analyzedExpr.generateIr(opt.Controlflow(() => patternCtrl.op), globalVars, funs, Map.empty, Counter())
    lazy val (patternCtrl: opt.Controlflow, data: List[(opt.Datatype, opt.Dataflow)]) = Pattern.generateIrGlobal(analyzedPattern, exprData, retCtrl, globalVars)
    lazy val retOp = opt.Ret(data.map(_._2))
    lazy val retCtrl = opt.Controlflow(() => retOp)
    opt.Var(exprCtrl, data.map(_._1).toArray)
  }

  def gatherFuns: List[Fun] = analyzedExpr.gatherFuns

  lazy val runtime: Boolean = pattern.datatype.runtime && analyzedExpr.runtime

  lazy val compiletime: Boolean = analyzedExpr.compiletime

  def typeCheck(): Unit = {
    if (analyzedExpr.returnType !~=> analyzedPattern.datatype) throw Error.typeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
  }

  def format(indentation: Int): String = s"${analyzedExpr.constVal.map(value => s"${" " * indentation}[CONST: ${value.toString}]\n").getOrElse("")}${" " * indentation}${pattern.format(indentation)} = ${analyzedExpr.format(indentation)};"
}

abstract class Fun {
  def range: FilePosRange

  def argTypes: List[Datatype]

  def returnType: Datatype

  def format(indentation: Int): String

  def constEval(args: List[ConstVal]): Option[ConstVal]

  def generateCode(): List[Instr]

  def generateInlineCode(ctx: ExprCodeGenContext): Boolean

  def generateIr(globalVars: Map[GlobalVar, Int], funs: Map[Fun, Int]): opt.Fun

  def runtime: Boolean

  def compiletime: Boolean

  def gatherFuns: List[Fun]

  lazy val signature: FunDatatype = FunDatatype(argTypes.map(_.withMut(false)), returnType.withMut(false), false)

  private var labelInternal: Option[String] = None

  def label: String = labelInternal match {
    case Some(label) => label
    case None =>
      val label = AsmGen.functionLabel()
      labelInternal = Some(label)
      AsmGen.function(label, generateCode())
      label
  }
}

class BuiltinFun(val module: Module, val argTypes: List[Datatype], val returnType: Datatype, eval: Option[List[ConstVal] => ConstVal], generate: () => List[Instr], graph: => Option[opt.Fun], inlineGenerate: ExprCodeGenContext => Boolean = _ => false) extends Fun {
  override def range: FilePosRange = module.file.lastRange

  override def format(indentation: Int): String = s"builtin fun(${argTypes.mkString(", ")}): $returnType"

  override def constEval(args: List[ConstVal]): Option[ConstVal] = eval.map(_(args))

  override def generateCode(): List[Instr] = generate()

  override def generateInlineCode(ctx: ExprCodeGenContext): Boolean = inlineGenerate(ctx)

  override def generateIr(globalVars: Map[GlobalVar, Int], funs: Map[Fun, Int]): opt.Fun = graph.get

  lazy val runtime: Boolean = signature.runtime && graph.nonEmpty

  lazy val compiletime: Boolean = eval.nonEmpty

  override def gatherFuns: List[Fun] = List.empty
}

class UserFun(val module: Module, parameters: List[syn.Pattern], retTypeExpr: Option[syn.Expr], expr: syn.Expr, val range: FilePosRange) extends Fun {
  lazy val (args: List[Pattern[LocalVar]], params: Map[String, LocalVar]) = {
    val (args, params) = parameters.map(parameter => Pattern.analyze((pattern, patternNav) => new LocalVar(module, pattern.name, None, patternNav, pattern.datatype, pattern.range), parameter)).unzip
    (args, Pattern.verify(params.flatten))
  }

  lazy val analyzedExpr: Expr = {
    val analyzedExpr = analyzeExpr(new ExprParsingContext(module, Some(this)))(expr)
    for {
      returnType <- annotatedReturnType
      if analyzedExpr.returnType !~=> returnType
    } throw Error.typeMismatch(analyzedExpr.returnType, returnType, analyzedExpr.range, retTypeExpr.get.range)
    analyzedExpr
  }

  lazy val argTypes: List[Datatype] = args.map(_.datatype)

  lazy val annotatedReturnType: Option[Datatype] = retTypeExpr.map(analyzeExpr(new ExprParsingContext(module, None))(_).constDatatype)

  lazy val returnType: Datatype = annotatedReturnType.getOrElse(analyzedExpr.returnType)

  override def format(indentation: Int): String = s"fn(${args.map(_.format(indentation)).mkString(", ")}): $returnType => ${analyzedExpr.format(indentation)}"

  override def constEval(arguments: List[ConstVal]): Option[ConstVal] = {
    val ctx = new ExprConstEvalContext(module)
    args.zip(arguments).foreach(t => ctx.add(t._1, t._2))
    analyzedExpr.constEval(ctx)
  }

  override def generateCode(): List[Instr] = {
    val ctx = new ExprCodeGenContext()

    params.values.foreach(ctx.add)

    def iterateArgs(arg: Pattern[LocalVar], offset: Int): Unit = arg match {
      case VarPattern(patternVar, _) => patternVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.lookup(patternVar), Reg.RBP + offset)
      case TuplePattern(elements, _) => elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3.map(_ + offset)).foreach(iterateArgs.tupled)
    }

    args.foldLeft(0) { (offset, arg) =>
      iterateArgs(arg, offset)
      offset + arg.datatype.size
    }

    analyzedExpr.generateCode(ctx)

    ctx.add(Ret())
    ctx.code
  }

  override def generateInlineCode(ctx: ExprCodeGenContext): Boolean = false

  override def generateIr(globalVars: Map[GlobalVar, Int], funs: Map[Fun, Int]): opt.Fun = {
    val counter = Counter()
    lazy val (exprData: opt.Dataflow, exprCtrl: opt.Controlflow) = analyzedExpr.generateIr(retCtrl, globalVars, funs, Map.empty, counter)
    lazy val retOp = opt.Ret(List(exprData))
    lazy val retCtrl = opt.Controlflow(() => retOp)
    opt.CodeFun(exprCtrl, signature.optDatatype.asInstanceOf[opt.FunDatatype], counter.localVars)
  }

  lazy val runtime: Boolean = signature.runtime && analyzedExpr.runtime

  lazy val compiletime: Boolean = analyzedExpr.compiletime

  override def gatherFuns: List[Fun] = this :: analyzedExpr.gatherFuns
}

class Module(val file: File, fileContent: => (Map[String, List[GlobalVar]], List[UserGlobalVarInit])) {
  lazy val (vars, varInits) = fileContent

  def format(indentation: Int): String = {
    def section(string: String) = string match {
      case "" => ""
      case string => "\n" + string
    }

    val formattedBuiltin = section(vars.filter(_._2.isInstanceOf[BuiltinGlobalVar]).map(v => s"${v._2.asInstanceOf[BuiltinGlobalVar].format(indentation + 1)}\n").mkString)
    val formattedVars = section(varInits.map(v => s"${v.format(indentation + 1)}\n\n").mkString)
    s"${" " * indentation}module ${file.name} {\n$formattedBuiltin$formattedVars}"
  }

  def generateIr(): opt.OptUnit = {
    val globalVars = vars.values.flatten
    val funs = varInits.flatMap(_.gatherFuns)

    val globalVarsMap = globalVars.zipWithIndex.toMap
    val funsMap = funs.zipWithIndex.toMap

    val varNodes = varInits.map(_.generateIr(globalVarsMap, funsMap))
    val funNodes = funs.map(_.generateIr(globalVarsMap, funsMap))
//    vars("main").head.asInstanceOf[UserGlobalVar].initt.generateIr(globalVars, funs)
    opt.OptUnit(funNodes.toArray, varNodes.toArray)
  }
}
