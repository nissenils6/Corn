package sem

import core.*
import gen.*

import scala.collection.mutable

abstract class Var {
  def name: String

  def range: FilePosRange

  def datatype: Datatype
}

abstract class GlobalVar extends Var {
  def constVal: Option[ConstVal]

  lazy val label: Option[String] = if datatype.runtime then constVal match {
    case Some(value) => Some(AsmGen.data(value)._1)
    case None => Some(AsmGen.bss(datatype.size, datatype.align)._1)
  } else None
}

class BuiltinGlobalVar(module: => Module, val name: String, value: ConstVal) extends GlobalVar {
  override def range: FilePosRange = module.file.lastRange

  lazy val datatype: Datatype = value.datatype
  lazy val constVal: Option[ConstVal] = Some(value)

  def format(indentation: Int): String = s"${" " * indentation}builtin $name: $datatype = $value"
}

class UserGlobalVar(module: => Module, val name: String, init: => UserGlobalVarInit, patternNav: PatternNav, typeExpr: Option[syn.Expr], val range: FilePosRange) extends GlobalVar {
  lazy val datatype: Datatype = typeExpr.map(typeExpr => analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype).getOrElse(patternNav.datatype(init.analyzedExpr.returnType))
  lazy val constVal: Option[ConstVal] = if datatype.mutable then None else init.analyzedExpr.constVal.map(patternNav.const)
}

class UserGlobalVarInit(module: => Module, expr: syn.Expr, pattern: => Pattern[UserGlobalVar]) {
  lazy val analyzedExpr: Expr = analyzeExpr(ExprParsingContext(module, None))(expr)
  lazy val analyzedPattern: Pattern[UserGlobalVar] = pattern

  def typeCheck(): Unit = {
    if (analyzedPattern.datatype !~= analyzedExpr.returnType) throw Error.typeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
  }

  def format(indentation: Int): String = s"${analyzedExpr.constVal.map(value => s"${" " * indentation}[CONST: ${value.toString}]\n").getOrElse("")}${" " * indentation}${pattern.format(indentation)} = ${analyzedExpr.format(indentation)};"
}

abstract class Fun {
  def range: FilePosRange

  def argTypes: List[Datatype]

  def argNameToIndex: Map[String, Int]

  def returnType: Datatype

  def format(indentation: Int): String

  def constEval(args: List[ConstVal]): Option[ConstVal]

  def generateCode(): List[Instr]

  def generateInlineCode(ctx: ExprCodeGenContext): Boolean

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

class BuiltinFun(val module: Module, val argTypes: List[Datatype], val returnType: Datatype, eval: List[ConstVal] => Option[ConstVal], generate: () => List[Instr], inlineGenerate: ExprCodeGenContext => Boolean = _ => false) extends Fun {
  override def range: FilePosRange = module.file.lastRange

  override def argNameToIndex: Map[String, Int] = Map()

  override def format(indentation: Int): String = s"builtin fun(${argTypes.mkString(", ")}): $returnType"

  override def constEval(args: List[ConstVal]): Option[ConstVal] = eval(args)

  override def generateCode(): List[Instr] = generate()

  override def generateInlineCode(ctx: ExprCodeGenContext): Boolean = inlineGenerate(ctx)
}

class UserFun(val module: Module, parameters: List[syn.Pattern], retTypeExpr: Option[syn.Expr], expr: syn.Expr, val range: FilePosRange) extends Fun {
  lazy val (args: List[Pattern[LocalVar]], params: Map[String, LocalVar]) = {
    val (args, params) = parameters.map(parameter => Pattern.map((pattern, patternNav) => new LocalVar(module, pattern.name, None, patternNav, pattern.datatype, pattern.range), parameter)).unzip
    (args, Pattern.verify(params.flatten))
  }

  lazy val analyzedExpr: Expr = analyzeExpr(new ExprParsingContext(module, Some(this)))(expr)

  lazy val argTypes: List[Datatype] = args.map(_.datatype)
  lazy val argNameToIndex: Map[String, Int] = args.zipWithIndex.flatMap {
    case (VarPattern(patternVar, _), index) => List((patternVar.name, index))
    case _ => List()
  }.toMap

  lazy val returnType: Datatype = retTypeExpr match {
    case Some(retTypeExpr) =>
      val returnType = analyzeExpr(new ExprParsingContext(module, None))(retTypeExpr).constDatatype
      if (returnType != analyzedExpr.returnType) throw Error.typeMismatch(analyzedExpr.returnType, returnType, analyzedExpr.range, retTypeExpr.range)
      returnType
    case None => analyzedExpr.returnType
  }

  override def format(indentation: Int): String = s"fn(${args.map(_.format(indentation)).mkString(", ")}): $returnType => ${analyzedExpr.format(indentation)}"

  override def constEval(arguments: List[ConstVal]): Option[ConstVal] = {
    val ctx = new ExprConstEvalContext()
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

    args.foldLeft(0) { case (offset, arg) =>
      iterateArgs(arg, offset)
      offset + arg.datatype.size
    }

    analyzedExpr.generateCode(ctx)

    ctx.add(Ret())
    ctx.code
  }

  override def generateInlineCode(ctx: ExprCodeGenContext): Boolean = false
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
}
