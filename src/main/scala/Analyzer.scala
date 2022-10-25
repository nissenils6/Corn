import scala.annotation.tailrec
import scala.collection.mutable

trait AnalyzerVar {
  def name: String
  def datatype: Datatype
}

abstract class AnalyzerPattern[T <: AnalyzerVar] {
  def range: FilePosRange
  def datatype: Datatype

  override def toString: String = this match {
    case AnalyzerVarPattern(patternVar, _) => s"${patternVar.name}: $datatype"
    case AnalyzerTuplePattern(content, _) => s"(${content.mkString(", ")})"
  }
}

case class AnalyzerVarPattern[T <: AnalyzerVar](patternVar: T, range: FilePosRange) extends AnalyzerPattern[T] {
  override def datatype: Datatype = patternVar.datatype
}

case class AnalyzerTuplePattern[T <: AnalyzerVar](content: List[AnalyzerPattern[T]], range: FilePosRange) extends AnalyzerPattern[T] {
  override def datatype: Datatype = TupleDatatype(content.map(_.datatype))
}

abstract class ConstVal(val datatype: Datatype) {

}

case class ConstInt(int: Int) extends ConstVal(IntDatatype())

case class ConstType(valDatatype: Datatype) extends ConstVal(TypeDatatype())

case class ConstTuple(elements: List[ConstVal]) extends ConstVal(TupleDatatype(elements.map(_.datatype)))

abstract class Datatype(val size: Int, val align: Int) {
  def generateCopyCode(asmBuilder: AsmBuilder, dstReg: String, dstOffset: Int, srcReg: String, srcOffset: Int): Unit
}

case class UnitDatatype() extends Datatype(0, 0) {
  override def generateCopyCode(asmBuilder: AsmBuilder, dstReg: String, dstOffset: Int, srcReg: String, srcOffset: Int): Unit = ()
}

case class IntDatatype() extends Datatype(8, 8) {
  override def generateCopyCode(asmBuilder: AsmBuilder, dstReg: String, dstOffset: Int, srcReg: String, srcOffset: Int): Unit = {
    asmBuilder.line(s"mov rsi [$srcReg + $srcOffset]")
    asmBuilder.line(s"mov [$dstReg + $dstOffset]")
  }
}

case class TypeDatatype() extends Datatype(0, 0){
  override def generateCopyCode(asmBuilder: AsmBuilder, dstReg: String, dstOffset: Int, srcReg: String, srcOffset: Int): Unit = ()
}

case class TupleDatatype(elements: List[Datatype]) extends Datatype(elements.map(_.size).sum, elements.map(_.align).max) {
  override def generateCopyCode(asmBuilder: AsmBuilder, dstReg: String, dstOffset: Int, srcReg: String, srcOffset: Int): Unit = {
    var offset = 0
    for (datatype <- elements) {
      datatype.generateCopyCode(asmBuilder, dstReg, dstOffset + offset, srcReg, srcOffset + offset)
      offset += datatype.size
    }
  }
}

case class FunDatatype(params: List[Datatype], returnType: Datatype) extends Datatype(8, 8){
  override def generateCopyCode(asmBuilder: AsmBuilder, dstReg: String, dstOffset: Int, srcReg: String, srcOffset: Int): Unit = {
    asmBuilder.line(s"mov rsi [$srcReg + $srcOffset]")
    asmBuilder.line(s"mov [$dstReg + $dstOffset]")
  }
}

abstract class AnalyzerExpr() {
  def range: FilePosRange
  def constEval: Option[ConstVal]
  def returnType: Datatype
  def generateCode(ctx: CodeGenContext): Unit
}

case class AnalyzerCallExpr(function: AnalyzerExpr, args: List[AnalyzerExpr], range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = function.returnType.asInstanceOf[FunDatatype].returnType
}

case class AnalyzerGlobalVarRefExpr(globalVar: GlobalVar, range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = globalVar.datatype
}

case class AnalyzerGlobalFunRefExpr(globalFun: GlobalFun, range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = FunDatatype(globalFun.args.map(_.datatype), globalFun.returnType)
}

case class AnalyzerIntExpr(int: Int, range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = IntDatatype()
}

case class AnalyzerTupleExpr(content: List[AnalyzerExpr], range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = TupleDatatype(content.map(_.returnType))
}

case class AnalyzerBlockExpr(exprs: List[AnalyzerExpr], lastExpr: AnalyzerExpr, range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = lastExpr.returnType
}

case class AnalyzerUnitExpr(range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = UnitDatatype()
}

case class AnalyzerLetExpr(pattern: AnalyzerPattern[LocalVar], expr: AnalyzerExpr, range: FilePosRange) extends AnalyzerExpr {
  override def returnType: Datatype = expr.returnType
}

class GlobalVar(val module: Module, val name: String, val init: GlobalVarInit, val typeExpr: Expr) extends AnalyzerVar {
  private var analyzedTypeExpr: Option[AnalyzerExpr] = None

  def datatype: Datatype = analyzedTypeExpr.getOrElse({
    analyzedTypeExpr = Some(analyzeExpr(ExprParsingContext(module, None, List()))(typeExpr)._1)
    analyzedTypeExpr.get
  }).constEval match {
    case ConstType(datatype) => datatype
    case _ => throw Error.unimplemented(module.file)
  }
}

class GlobalVarInit(val module: Module) {
  var pattern: AnalyzerPattern[GlobalVar]
}

case class ExprParsingContext(module: Module, fun: Option[GlobalFun], vars: List[AnalyzerVar]) {
  def lookup(name: String): Option[Either[AnalyzerVar, GlobalFunTable]] = {
    @tailrec
    def rec(list: List[AnalyzerVar]): Option[Either[AnalyzerVar, GlobalFunTable]] = if (list.isEmpty) {
        fun.flatMap(_.params.get(name)).orElse(module.vars.get(name)).map(v => Left(v)).orElse(module.funTables.get(name).map(t => Right(t)))
      } else if (list.head.name == name) {
        Some(Left(list.head))
      } else {
        rec(list.tail)
      }

    rec(vars)
  }

  def withVar(analyzerVar: AnalyzerVar): ExprParsingContext = ExprParsingContext(module, fun, analyzerVar :: vars)
}

case class CodeGenContext(module: Module, stackOffset: Int) {

}

class LocalVar(val module: Module, val name: String, val typeExpr: Expr) extends AnalyzerVar {
  private var analyzedTypeExpr: Option[AnalyzerExpr] = None
  var stackOffset: Option[Int] = None

  def datatype: Datatype = analyzedTypeExpr.getOrElse({
    analyzedTypeExpr = Some(analyzeExpr(ExprParsingContext(module, None, List()))(typeExpr)._1)
    analyzedTypeExpr.get
  }).constEval match {
    case ConstType(datatype) => datatype
    case _ => throw Error.unimplemented(module.file)
  }
}

class GlobalFun(val module: Module, val name: String, val retTypeExpr: Expr, val expr: Expr) {
  private var analyzedRetTypeExpr: Option[AnalyzerExpr] = None
  val params: mutable.Map[String, LocalVar] = mutable.Map()
  var args: List[AnalyzerPattern[LocalVar]]
  var funBuilder: Option[AsmBuilder] = None

  def returnType: Datatype = analyzedRetTypeExpr.getOrElse({
    analyzedRetTypeExpr = Some(analyzeExpr(ExprParsingContext(module, None, List()))(retTypeExpr)._1)
    analyzedRetTypeExpr.get
  }).constEval match {
    case ConstType(datatype) => datatype
    case _ => throw Error.unimplemented(module.file)
  }

  def generateCode(): Unit = if (funBuilder.isEmpty) {
    val asmBuilder = codeGen.createFun()
    funBuilder = Some(asmBuilder)

    asmBuilder.line(s"push rbp")
    asmBuilder.line(s"mov rbp, rsp")

    var paramsSize = 0
    for (param <- params.values) {
      param.stackOffset = Some(paramsSize)
      paramsSize += param.datatype.size
    }
    val argsSize = args.map(_.datatype.size).sum

    def iterArgPattern(pattern: AnalyzerPattern[LocalVar], argStackOffset: Int): Unit = pattern match {
      case AnalyzerVarPattern(patternVar, _) =>
        patternVar.datatype.generateCopyCode(asmBuilder, "rbp", patternVar.stackOffset.get, "rbp", argStackOffset)
      case AnalyzerTuplePattern(content, _) =>
        var argOffset = argStackOffset
        for (subPattern <- content) {
          iterArgPattern(subPattern, argOffset)
          argOffset += subPattern.datatype.size
        }
    }

    {
      var argOffset = -16 - argsSize
      for (subPattern <- args) {
        iterArgPattern(subPattern, argOffset)
        argOffset += subPattern.datatype.size
      }
    }

    val ctx = CodeGenContext(module, paramsSize)
    asmBuilder.line(s"add rsp, $paramsSize")

    val analyzedExpr = analyzeExpr(ExprParsingContext(module, Some(this), List()))(expr)._1
    analyzedExpr.generateCode(ctx)

    asmBuilder.line(s"sub rsp, $paramsSize")
    asmBuilder.line(s"pop rbp")
    asmBuilder.line(s"ret")
  }
}

class GlobalFunTable(val module: Module, val name: String) {
  val funs = mutable.Buffer[GlobalFun]()
}

class Module(val file: File) {
  val vars = mutable.Map[String, GlobalVar]()
  val funTables = mutable.Map[String, GlobalFunTable]()
}

def analyzeExpr(ctx: ExprParsingContext)(expr: Expr): (AnalyzerExpr, ExprParsingContext) = expr match {
  case CallExpr(function, posArgs, keywordArgs, range) =>
    val (analyzedFunExpr, _) = analyzeExpr(ctx)(function)
    analyzedFunExpr.returnType match {
      case FunDatatype(params, returnType) =>
        if(params.length != posArgs.length) {
          throw Error.unimplemented(range.file)
        }
        val analyzedArgs = posArgs.map(analyzeExpr(ctx)).map(_._1)
        if (!params.zip(analyzedArgs.map(_.returnType)).forall(t => t._1 == t._2)) {
          throw Error.unimplemented(range.file)
        }
        (AnalyzerCallExpr(analyzedFunExpr, analyzedArgs, range), ctx)
      case _ => throw Error.unimplemented(range.file)
    }
  case RefExpr(iden, range) => ctx.lookup(iden) match {
    case Some(v) => v match {
      case Right(globalFun: GlobalFun) => (AnalyzerGlobalFunRefExpr(globalFun, range), ctx)
      case Left(globalVar: GlobalVar) => (AnalyzerGlobalVarRefExpr(globalVar, range), ctx)
      case Left(parameterVar: LocalVar) => ???
    }
  }
  case IntExpr(int, range) => (AnalyzerIntExpr(int, range), ctx)
  case TupleExpr(content, range) => (AnalyzerTupleExpr(content.map(analyzeExpr(ctx)).map(_._1), range), ctx)
  case BlockExpr(exprs, lastExpr, range) => (AnalyzerBlockExpr({
    var curCtx = ctx
    for (expr <- exprs) yield {
      val (analyzedExpr, newCtx) = analyzeExpr(curCtx)(expr)
      curCtx = newCtx
      analyzedExpr
    }
  }, analyzeExpr(ctx)(lastExpr)._1, range), ctx)
  case UnitExpr(range) => (AnalyzerUnitExpr(range), ctx)
  case LetExpr(pattern, expr, range) => {
    val (analyzedPattern, newCtx) = iterateLocalPattern(ctx)(pattern)
    (AnalyzerLetExpr(analyzedPattern, analyzeExpr(ctx)(expr)._1, range), newCtx)
  }
}

def iterateGlobalPattern(init: GlobalVarInit)(pattern: Pattern): AnalyzerPattern[GlobalVar] = pattern match {
  case VarPattern(name, datatype, range) =>
    if (init.module.vars.contains(name)) {
      throw Error.duplicate(name, range)
    }
    val globalVar = new GlobalVar(init.module, name, init, datatype)
    init.module.vars(name) = globalVar
    AnalyzerVarPattern[GlobalVar](globalVar, range)
  case TuplePattern(content, range) => AnalyzerTuplePattern[GlobalVar](content.map(iterateGlobalPattern(init)), range)
}

def iterateArgPattern(fun: GlobalFun)(pattern: Pattern): AnalyzerPattern[LocalVar] = pattern match {
  case VarPattern(name, datatype, range) =>
    if (fun.params.contains(name)) {
      throw Error.duplicate(name, range)
    }
    val localVar = new LocalVar(fun.module, name, datatype)
    fun.params(name) = localVar
    AnalyzerVarPattern[LocalVar](localVar, range)
  case TuplePattern(content, range) => AnalyzerTuplePattern[LocalVar](content.map(iterateArgPattern(fun)), range)
}

def iterateLocalPattern(ctx: ExprParsingContext)(pattern: Pattern): (AnalyzerPattern[LocalVar], ExprParsingContext) = pattern match {
  case VarPattern(name, datatype, range) =>
    val localVar = new LocalVar(ctx.module, name, datatype)
    (AnalyzerVarPattern[LocalVar](localVar, range), ctx withVar localVar)
  case TuplePattern(content, range) =>
    var curCtx = ctx
    (AnalyzerTuplePattern(for (subPattern <- content) yield {
      val (analyzedPattern, newCtx) = iterateLocalPattern(curCtx)(subPattern)
      curCtx = newCtx
      analyzedPattern
    }, range), curCtx)
}

def analyzeFile(stmts: List[GlobalStmt], file: File): Module = {
  val module: Module = new Module(file)

  for (stmt <- stmts) stmt match {
    case FunGlobalStmt(name, parameters, returnType, expr, range) =>
      val fun = GlobalFun(module, name, returnType, expr)
      if (!module.funTables.contains(name))
        module.funTables(name) = GlobalFunTable(module, name)
      module.funTables(name).funs.append(fun)
      fun.args = parameters.map(iterateArgPattern(fun))
    case LetGlobalStmt(pattern, expr, range) =>
      val init = GlobalVarInit(module)
      val analyzerPattern = iterateGlobalPattern(init)(pattern)
      init.pattern = analyzerPattern
  }

  if (!module.funTables.contains("main")) {
    throw Error.unimplemented(module.file)
  }

  val mainFunTable = module.funTables("main")
  val mainFuns = mainFunTable.funs.filter(_.args.isEmpty)

  if (mainFuns.length > 1) {
    throw Error.unimplemented(module.file)
  }

  val mainFun = mainFuns.head

  mainFun.generateCode()

  module
}
