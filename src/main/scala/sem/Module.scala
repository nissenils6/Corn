package sem

import core.*
import gen.*
import opt.BssElement

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
}

class UserGlobalVarInit(module: => Module, expr: syn.Expr, pattern: => Pattern[UserGlobalVar]) {
  lazy val analyzedExpr: Expr = analyzeExpr(ExprParsingContext(module, None))(expr)
  lazy val analyzedPattern: Pattern[UserGlobalVar] = pattern

  def generateIr(context: IrGenContext): (opt.Var, List[GlobalVar]) = {
    val localVars = analyzedExpr.gatherLocals
    lazy val (exprData: opt.Dataflow, exprCtrl: opt.Controlflow) = analyzedExpr.generateIr(opt.Controlflow(() => patternCtrl.op), context, localVars.zipWithIndex.toMap)
    lazy val (patternCtrl: opt.Controlflow, data: List[(GlobalVar, opt.Datatype, opt.Dataflow)]) = Pattern.generateIrGlobal(analyzedPattern, exprData, retCtrl)
    lazy val retOp = opt.Ret(data.map(_._3))
    lazy val retCtrl = opt.Controlflow(() => retOp)
    (opt.Var(exprCtrl, data.map(_._2).toArray, localVars.map(_.datatype.optDatatype)), data.map(_._1))
  }

  def gatherFuns(funs: mutable.Set[Fun]): Unit = analyzedExpr.gatherFuns(funs)

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

  def generateIr(context: IrGenContext): opt.Fun

  def runtime: Boolean

  def compiletime: Boolean

  def gatherFuns(funs: mutable.Set[Fun]): Unit

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

  override def constEval(args: List[ConstVal]): Option[ConstVal] = eval.map(_ (args))

  override def generateCode(): List[Instr] = generate()

  override def generateInlineCode(ctx: ExprCodeGenContext): Boolean = inlineGenerate(ctx)

  override def generateIr(context: IrGenContext): opt.Fun = graph.get

  lazy val runtime: Boolean = signature.runtime && graph.nonEmpty

  lazy val compiletime: Boolean = eval.nonEmpty

  override def gatherFuns(funs: mutable.Set[Fun]): Unit = funs.add(this)
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

  override def generateIr(context: IrGenContext): opt.Fun = {
    val localVarsList = analyzedExpr.gatherLocals.concat(params.values)
    val localVars = localVarsList.zipWithIndex.toMap
    lazy val patternCtrl: opt.Controlflow = args.zipWithIndex.foldRight(exprCtrl)((tuple, next) => {
      val (pattern, index) = tuple
      Pattern.generateIrLocal(pattern, opt.Dataflow(() => None, index), next, localVars)
    })
    lazy val (exprData: opt.Dataflow, exprCtrl: opt.Controlflow) = analyzedExpr.generateIr(retCtrl, context, localVars)
    lazy val retOp = opt.Ret(List(exprData))
    lazy val retCtrl = opt.Controlflow(() => retOp)
    opt.CodeFun(patternCtrl, signature.optDatatype.asInstanceOf[opt.FunDatatype], localVarsList.map(_.datatype.optDatatype))
  }

  lazy val runtime: Boolean = signature.runtime && analyzedExpr.runtime

  lazy val compiletime: Boolean = analyzedExpr.compiletime

  override def gatherFuns(funs: mutable.Set[Fun]): Unit = if (!funs.contains(this)) {
    funs.add(this)
    analyzedExpr.gatherFuns(funs)
  }
}

object IrGenBuiltin {
  val bssSection: Map[String, BssElement] = Map(
    "println_buffer" -> BssElement(32, 1)
  )

  val constSection: Map[String, opt.ConstElement] = Map(

  )

  val dataSection: Map[String, opt.DataElement] = Map(

  )

  val windowsFunctions: List[String] = List("GetStdHandle", "WriteFile")

  val printlnFun: opt.AsmFun = opt.AsmFun(List(
    Load(Reg.RAX, Address(Reg.RBP)),
    Lea(Reg.RBX, Address("println_buffer") + 32),
    Sub(Reg.RBX, 1),
    StoreImm(Address(Reg.RBX), 10, RegSize.Byte),

    Mov(Reg.RSI, Reg.RAX),
    Cmp(Reg.RAX, 0),
    DirCondJump("println_no_neg", Flag.GreaterOrEqual),
    Neg(Reg.RAX),

    Label("println_no_neg"),
    LoadImm(Reg.RCX, 10, RegSize.QWord),

    Label("println_loop"),
    Xor(Reg.RDX, Reg.RDX),
    Idiv(Reg.RCX),
    Add(Reg.RDX, 48),
    Sub(Reg.RBX, 1),
    Store(Address(Reg.RBX), Reg.RDX, RegSize.Byte),
    Cmp(Reg.RAX, 0),
    DirCondJump("println_loop", Flag.Greater),

    Cmp(Reg.RSI, 0),
    DirCondJump("println_no_minus", Flag.GreaterOrEqual),
    Sub(Reg.RBX, 1),
    StoreImm(Address(Reg.RBX), 45, RegSize.Byte),

    Label("println_no_minus"),
    Sub(Reg.RSP, 48),
    LoadImm(Reg.RCX, -11),
    IndCall(Address("GetStdHandle")),
    Mov(Reg.RCX, Reg.RAX),
    Mov(Reg.RDX, Reg.RBX),
    Lea(Reg.R8, Address("println_buffer") + 32),
    Sub(Reg.R8, Reg.RBX),
    Lea(Reg.R9, Reg.RSP + 32),
    StoreImm(Reg.RSP + 32, 0),
    IndCall(Address("WriteFile")),
    Add(Reg.RSP, 48),
    Ret()
  ), opt.FunDatatype(List(opt.IntDatatype), List(opt.UnitDatatype)))

  val staticData: opt.StaticData = opt.StaticData(bssSection, constSection, dataSection, windowsFunctions)
}

case class IrGenContext(funs: () => Map[Fun, () => opt.Fun], globalVars: () => Map[GlobalVar, () => (opt.Var, Int)]) {
  def apply(fun: Fun): opt.Fun = funs()(fun)()
  
  def apply(globalVar: GlobalVar): (opt.Var, Int) = globalVars()(globalVar)()
}

class Module(val file: File, fileContent: => (Map[String, List[GlobalVar]], List[UserGlobalVarInit])) {
  lazy val (vars, varInits) = fileContent

  def format(indentation: Int): String = {
    def section(string: String) = string match {
      case "" => ""
      case string => "\n" + string
    }

    val formattedBuiltin = section(vars.values.flatten.filter(_.isInstanceOf[BuiltinGlobalVar]).map(v => s"${v.asInstanceOf[BuiltinGlobalVar].format(indentation + 1)}\n").mkString)
    val formattedVars = section(varInits.map(v => s"${v.format(indentation + 1)}\n\n").mkString)
    s"${" " * indentation}module ${file.name} {\n$formattedBuiltin$formattedVars}"
  }

  def generateIr(): opt.OptUnit = {
    lazy val globalInitsMap = varInits.map { varInit =>
      lazy val (irVar: opt.Var, globalVars: List[GlobalVar]) = varInit.generateIr(context)
      (varInit, () => (irVar, globalVars))
    }.toMap

    lazy val globalVarsMap = varInits.flatMap { varInit =>
      lazy val (irVar: opt.Var, globalVars: List[GlobalVar]) = globalInitsMap(varInit)()
      globalVars.zipWithIndex.map { case (globalVar, index) => (globalVar, () => (irVar, index)) }
    }.concat(vars.values.flatten.collect { case builtin: BuiltinGlobalVar if builtin.datatype.runtime && builtin.constVal.nonEmpty =>
      lazy val (constValData: opt.Dataflow, constValCtrl: opt.Controlflow) = builtin.constVal.get.generateIr(context).toGraph(retCtrl)
      lazy val retOp: opt.Op = opt.Ret(List(constValData))
      lazy val retCtrl: opt.Controlflow = opt.Controlflow(() => retOp)
      lazy val irVar: opt.Var = opt.Var(constValCtrl, Array(builtin.datatype.optDatatype), List())
      (builtin, () => (irVar, 0))
    }).toMap

    lazy val funsMap = {
      val funs = mutable.Set[Fun]()
      varInits.foreach(_.gatherFuns(funs))
      vars.values.flatten.foreach {
        case builtin: BuiltinGlobalVar => builtin.constVal.foreach(_.gatherFuns(funs))
        case _ => ()
      }
      funs.map { fun =>
        lazy val irFun: opt.Fun = fun.generateIr(context)
        (fun, () => irFun)
      }.toMap
    }

    lazy val context: IrGenContext = IrGenContext(() => funsMap, () => globalVarsMap)

    val varNodes = globalVarsMap.values.map(_.apply()._1).toList
    val funNodes = funsMap.values.map(_.apply()).toList
    opt.OptUnit(funNodes, varNodes, IrGenBuiltin.staticData)
  }
}
