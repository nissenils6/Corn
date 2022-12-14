package sem

import core.*

import scala.collection.mutable

//abstract class Var {
//  def name: String
//
//  def range: FilePosRange
//
//  def datatype: Datatype
//
//  def constVal: Option[ConstVal]
//}
//
//abstract class GlobalVar extends Var {
//  def constVal: Option[ConstVal]
//
//  def runtime: Boolean
//
//  def compiletime: Boolean
//}
//
//class BuiltinGlobalVar(module: => Module, val name: String, value: ConstVal) extends GlobalVar {
//  override def range: FilePosRange = module.file.lastRange
//
//  lazy val datatype: Datatype = value.datatype
//  lazy val constVal: Option[ConstVal] = Some(value)
//
//  lazy val runtime: Boolean = value.datatype.runtime
//
//  lazy val compiletime: Boolean = true
//
//  def format(indentation: Int): String = s"${" " * indentation}builtin $name: $datatype = $value"
//}
//
//class UserGlobalVar(module: => Module, val name: String, init: => UserGlobalVarInit, patternNav: PatternNav, typeExpr: Option[syn.TypeExpr], val range: FilePosRange) extends GlobalVar {
//  lazy val datatype: Datatype = typeExpr.map(typeExpr => analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype).getOrElse(patternNav.datatype(init.analyzedExpr.returnType).withMut(false))
//  lazy val constVal: Option[ConstVal] = if datatype.mutable then None else init.analyzedExpr.constVal.map(patternNav.const)
//
//  lazy val runtime: Boolean = init.runtime
//
//  lazy val compiletime: Boolean = init.compiletime
//}
//
//class UserGlobalVarInit(module: => Module, expr: syn.Expr, pattern: => Pattern[UserGlobalVar]) {
//  lazy val analyzedExpr: Expr = analyzeExpr(ExprParsingContext(module, None))(expr)
//  lazy val analyzedPattern: Pattern[UserGlobalVar] = pattern
//
//  def generateIr(globalVar: opt.Var, context: IrGenContext): Unit = {
//    val localVars = analyzedExpr.gatherLocals
//    val (firstExprOp: opt.Op, lastExprOp: opt.OpNext) = analyzedExpr.generateIr(context, localVars.zipWithIndex.toMap)
//    val patternOps = Pattern.generateIrGlobal(analyzedPattern, opt.Data(lastExprOp), context)
//    val retOp = opt.Ret(List())
//    val firstPatternOp = opt.linkOps(retOp)(patternOps)
//
//    lastExprOp.next = firstPatternOp
//    globalVar.next = firstExprOp
//    globalVar.localVars = localVars.map(_.datatype.optDatatype)
//  }
//
//  def gatherFuns(funs: mutable.Set[Fun]): Unit = analyzedExpr.gatherFuns(funs)
//
//  lazy val runtime: Boolean = pattern.datatype.runtime && analyzedExpr.runtime
//
//  lazy val compiletime: Boolean = analyzedExpr.compiletime
//
//  def typeCheck(): Unit = {
//    if (analyzedExpr.returnType.isNotSubtypeOf(analyzedPattern.datatype)) throw Error.typeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
//  }
//
//  def format(indentation: Int): String = s"${analyzedExpr.constVal.map(value => s"${" " * indentation}[CONST: ${value.toString}]\n").getOrElse("")}${" " * indentation}${pattern.format(indentation)} = ${analyzedExpr.format(indentation)};"
//}
//
//abstract class Fun {
//  def range: FilePosRange
//
//  def argTypes: List[Datatype]
//
//  def returnType: Datatype
//
//  def format(indentation: Int): String
//
//  def constEval(args: List[ConstVal]): Option[ConstVal]
//
//  def runtime: Boolean
//
//  def compiletime: Boolean
//
//  def gatherFuns(funs: mutable.Set[Fun]): Unit
//
//  lazy val signature: FunDatatype = FunDatatype(argTypes.map(_.withMut(false)), returnType.withMut(false), false)
//}
//
//class BuiltinFun(val module: Module, val argTypes: List[Datatype], val returnType: Datatype, eval: Option[List[ConstVal] => ConstVal], graph: => Option[opt.Fun]) extends Fun {
//  override def range: FilePosRange = module.file.lastRange
//
//  override def format(indentation: Int): String = s"builtin fun(${argTypes.mkString(", ")}): $returnType"
//
//  override def constEval(args: List[ConstVal]): Option[ConstVal] = eval.map(_ (args))
//
//  def generateIr: opt.Fun = graph.get
//
//  lazy val runtime: Boolean = signature.runtime && graph.nonEmpty
//
//  lazy val compiletime: Boolean = eval.nonEmpty
//
//  override def gatherFuns(funs: mutable.Set[Fun]): Unit = funs.add(this)
//}
//
//class UserFun(val module: Module, parameters: List[syn.Pattern], retTypeExpr: Option[syn.Expr], expr: syn.Expr, val range: FilePosRange) extends Fun {
//  lazy val (args: List[Pattern[LocalVar]], params: Map[String, LocalVar]) = {
//    val (args, params) = parameters.map(parameter => Pattern.analyze((pattern, patternNav) => new LocalVar(module, pattern.name, None, patternNav, pattern.datatype, pattern.range), parameter)).unzip
//    (args, Pattern.verify(params.flatten))
//  }
//
//  lazy val analyzedExpr: Expr = {
//    val analyzedExpr = analyzeExpr(new ExprParsingContext(module, Some(this)))(expr)
//    for {
//      returnType <- annotatedReturnType
//      if analyzedExpr.returnType !~=> returnType
//    } throw Error.typeMismatch(analyzedExpr.returnType, returnType, analyzedExpr.range, retTypeExpr.get.range)
//    analyzedExpr
//  }
//
//  lazy val argTypes: List[Datatype] = args.map(_.datatype)
//
//  lazy val annotatedReturnType: Option[Datatype] = retTypeExpr.map(analyzeExpr(new ExprParsingContext(module, None))(_).constDatatype)
//
//  lazy val returnType: Datatype = annotatedReturnType.getOrElse(analyzedExpr.returnType)
//
//  override def format(indentation: Int): String = s"fn(${args.map(_.format(indentation)).mkString(", ")}): $returnType => ${analyzedExpr.format(indentation)}"
//
//  override def constEval(arguments: List[ConstVal]): Option[ConstVal] = {
//    val ctx = new ExprConstEvalContext(module)
//    args.zip(arguments).foreach(t => ctx.add(t._1, t._2))
//    analyzedExpr.constEval(ctx)
//  }
//
//  def generateIr(fun: opt.Fun, context: IrGenContext): Unit = {
//    val localVarsList = analyzedExpr.gatherLocals.concat(params.values)
//    val localVars = localVarsList.zipWithIndex.toMap
//    val paramOps = args.zipWithIndex.flatMap { case (pattern, index) =>
//      Pattern.generateIrLocal(pattern, opt.Data(None, index), localVars)
//    }
//    val (firstExprOp: opt.Op, lastExprOp: opt.OpNext) = analyzedExpr.generateIr(context, localVars)
//    val retOp = opt.Ret(List(opt.Data(lastExprOp)))
//    val firstParamOp = opt.linkOps(firstExprOp)(paramOps)
//
//    lastExprOp.next = retOp
//
//    fun.next = firstParamOp
//    fun.localVars = localVarsList.map(_.datatype.optDatatype)
//    fun.signature = signature.optDatatype.asInstanceOf[opt.FunDatatype]
//  }
//
//  lazy val runtime: Boolean = signature.runtime && analyzedExpr.runtime
//
//  lazy val compiletime: Boolean = analyzedExpr.compiletime
//
//  override def gatherFuns(funs: mutable.Set[Fun]): Unit = if (!funs.contains(this)) {
//    funs.add(this)
//    analyzedExpr.gatherFuns(funs)
//  }
//}
//
//case class IrGenContext(funs: Map[Fun, opt.Fun], globalVars: Map[GlobalVar, (opt.Var, Int)]) {
//  def apply(fun: Fun): opt.Fun = funs(fun)
//
//  def apply(globalVar: GlobalVar): (opt.Var, Int) = globalVars(globalVar)
//}
//
//class Module(val file: File, fileContent: => (Map[String, List[GlobalVar]], List[UserGlobalVarInit])) {
//  lazy val (vars, varInits) = fileContent
//
//  def format(indentation: Int): String = {
//    def section(string: String) = string match {
//      case "" => ""
//      case string => "\n" + string
//    }
//
//    val formattedBuiltin = section(vars.values.flatten.filter(_.isInstanceOf[BuiltinGlobalVar]).map(v => s"${v.asInstanceOf[BuiltinGlobalVar].format(indentation + 1)}\n").mkString)
//    val formattedVars = section(varInits.map(v => s"${v.format(indentation + 1)}\n\n").mkString)
//    s"${" " * indentation}module ${file.name} {\n$formattedBuiltin$formattedVars}"
//  }
//
//  def generateIr(): opt.OptUnit = {
//    val funs = mutable.Set[Fun]()
//    val (optVars: List[(UserGlobalVarInit, opt.Var)], globalVars: List[List[(GlobalVar, (opt.Var, Int))]]) = varInits.map { varInit =>
//      val optVar = new opt.Var()
//      varInit.gatherFuns(funs)
//
//      val t1 = (varInit, optVar)
//      val t2 = varInit.analyzedPattern.gatherVars.zipWithIndex.map { case (globalVar, index) =>
//        (globalVar, (optVar, index))
//      }
//
//      (t1, t2)
//    }.unzip
//
//    vars.values.flatten.foreach {
//      case builtin: BuiltinGlobalVar => builtin.constVal.get.gatherFuns(funs)
//      case _ => ()
//    }
//
//    val codeFuns = funs.map {
//      case userFun: UserFun =>
//        val codeFun = new opt.Fun()
//        (userFun, codeFun)
//      case builtinFun: BuiltinFun =>
//        (builtinFun, builtinFun.generateIr)
//    }
//    val funsMap = codeFuns.toMap
//
//    val builtinVars: List[(BuiltinGlobalVar, (opt.Var, Int))] = vars.values.flatten.collect { case builtin: BuiltinGlobalVar if builtin.datatype.runtime && builtin.constVal.nonEmpty =>
//      val optVar = new opt.Var()
//      val (firstConstOp: opt.Op, lastConstOp: opt.OpNext) = builtin.constVal.get.generateIr(funsMap).toGraph
//      val writeOp = opt.WriteGlobal(optVar, 0, opt.Data(lastConstOp))
//      val retOp = opt.Ret(List())
//
//      lastConstOp.next = writeOp
//      writeOp.next = retOp
//
//      optVar.next = firstConstOp
//      optVar.localVars = List()
//
//      (builtin, (optVar, 0))
//    }.toList
//
//    val globalVarsMap = globalVars.flatten.concat(builtinVars).toMap
//    val context = IrGenContext(funsMap, globalVarsMap)
//
//    optVars.foreach { case (varInit, optVar) =>
//      varInit.generateIr(optVar, context)
//    }
//
//    codeFuns.foreach {
//      case (userFun: UserFun, codeFun: opt.Fun) => userFun.generateIr(codeFun, context)
//      case _ => ()
//    }
//
//    opt.OptUnit(vars("main").find(_.datatype match {
//      case FunDatatype(Nil, UnitDatatype(_), _) => true
//      case _ => false
//    }) match {
//      case Some(globalVar: GlobalVar) => globalVar.constVal match {
//        case Some(ConstFunction(fun)) => funsMap(fun)
//      }
//    }, funsMap.values.toList, optVars.map(_._2).concat(builtinVars.map(_._2._1)))
//  }
//}
