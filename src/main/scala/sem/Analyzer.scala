package sem

import core.*
import gen.*

import scala.collection.mutable

def simpleOperator(module: => Module, name: String, compileTimeFunction: (Long, Long) => Long, asmOperation: (Address, Reg) => SimpleOpMem): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, name, ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), IntDatatype(false),
    args => Some(ConstInt(compileTimeFunction(args.head.toInt, args(1).toInt))),
    () => List(
      Load(Reg.RAX, Reg.RBP + 8),
      asmOperation(Address(Reg.RBP), Reg.RAX),
      Ret()
    ),
    ctx => {
      ctx.add(
        Load(Reg.RAX, Reg.RBP + ctx.secondaryOffset + 8),
        asmOperation(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
      )
      true
    }
  )))

def mulOperator(module: => Module): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, "*", ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), IntDatatype(false),
    args => Some(ConstInt(args.head.toInt * args(1).toInt)),
    () => List(
      Load(Reg.RAX, Address(Reg.RBP)),
      Load(Reg.RCX, Reg.RBP + 8),
      Imul(Reg.RAX, Reg.RCX),
      Store(Address(Reg.RBP), Reg.RAX),
      Ret()
    ),
    ctx => {
      ctx.add(
        Load(Reg.RAX, Reg.RBP + ctx.secondaryOffset),
        Load(Reg.RCX, Reg.RBP + ctx.secondaryOffset + 8),
        Imul(Reg.RAX, Reg.RCX),
        Store(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
      )
      true
    }
  )))

def divOperator(module: => Module, name: String, compileTimeFunction: (Long, Long) => Long, resultReg: Reg): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, name, ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), IntDatatype(false),
    args => Some(ConstInt(compileTimeFunction(args.head.toInt, args(1).toInt))),
    () => List(
      Load(Reg.RAX, Address(Reg.RBP)),
      Load(Reg.RCX, Reg.RBP + 8),
      Xor(Reg.RDX, Reg.RDX),
      Idiv(Reg.RCX),
      Store(Address(Reg.RBP), resultReg),
      Ret()
    ),
    ctx => {
      ctx.add(
        Load(Reg.RAX, Reg.RBP + ctx.secondaryOffset),
        Load(Reg.RCX, Reg.RBP + ctx.secondaryOffset + 8),
        Xor(Reg.RDX, Reg.RDX),
        Idiv(Reg.RCX),
        Store(Reg.RBP + ctx.secondaryOffset, resultReg)
      )
      true
    }
  )))

def comparisonOperator(module: => Module, name: String, compileTimeFunction: (Long, Long) => Boolean, flag: Flag): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, name, ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), BoolDatatype(false),
    args => Some(ConstBool(compileTimeFunction(args.head.toInt, args(1).toInt))),
    () => List(
      Load(Reg.RAX, Address(Reg.RBP)),
      Cmp(Reg.RAX, Reg.RBP + 8),
      SetCond(Reg.RCX, flag),
      Store(Address(Reg.RBP), Reg.RCX, RegSize.Byte),
      Ret()
    ),
    ctx => {
      ctx.add(
        Load(Reg.RAX, Reg.RBP + ctx.secondaryOffset),
        Cmp(Reg.RAX, Reg.RBP + ctx.secondaryOffset + 8),
        SetCond(Reg.RCX, flag),
        Store(Reg.RBP + ctx.secondaryOffset, Reg.RCX, RegSize.Byte)
      )
      true
    }
  )))

def builtinVars(module: => Module): List[GlobalVar] = List(
  new BuiltinGlobalVar(module, "Int", ConstType(IntDatatype(false))),
  new BuiltinGlobalVar(module, "Bool", ConstType(BoolDatatype(false))),
  new BuiltinGlobalVar(module, "Type", ConstType(TypeDatatype(false))),

  simpleOperator(module, "+", _ + _, Add.apply),
  simpleOperator(module, "-", _ - _, Sub.apply),
  simpleOperator(module, "&", _ & _, And.apply),
  simpleOperator(module, "|", _ | _, Or.apply),
  simpleOperator(module, "^", _ ^ _, Xor.apply),

  mulOperator(module),
  divOperator(module, "/", _ / _, Reg.RAX),
  divOperator(module, "%", _ % _, Reg.RDX),

  comparisonOperator(module, ">", _ > _, Flag.Greater),
  comparisonOperator(module, "<", _ < _, Flag.Less),
  comparisonOperator(module, ">=", _ >= _, Flag.GreaterOrEqual),
  comparisonOperator(module, "<=", _ <= _, Flag.LessOrEqual),
  comparisonOperator(module, "==", _ == _, Flag.Zero),
  comparisonOperator(module, "!=", _ != _, Flag.NotZero),

  new BuiltinGlobalVar(module, "println", ConstFunction(new BuiltinFun(module, List(IntDatatype(false)), UnitDatatype(false),
    _ => None,
    () => {
      val noNegLabel = AsmGen.label()
      val loopLabel = AsmGen.label()
      val noMinusLabel = AsmGen.label()

      val bufferLabel = AsmGen.bss(32)
      AsmGen.windowsFunction("WriteFile")
      AsmGen.windowsFunction("GetStdHandle")

      List(
        Load(Reg.RAX, Address(Reg.RBP)),
        Lea(Reg.RBX, Address(bufferLabel) + 32),
        Sub(Reg.RBX, 1),
        StoreImm(Address(Reg.RBX), 10, RegSize.Byte),

        Mov(Reg.RSI, Reg.RAX),
        Cmp(Reg.RAX, 0),
        DirCondJump(noNegLabel, Flag.GreaterOrEqual),
        Neg(Reg.RAX),

        Label(noNegLabel),
        LoadImm(Reg.RCX, 10, RegSize.QWord),

        Label(loopLabel),
        Xor(Reg.RDX, Reg.RDX),
        Idiv(Reg.RCX),
        Add(Reg.RDX, 48),
        Sub(Reg.RBX, 1),
        Store(Address(Reg.RBX), Reg.RDX, RegSize.Byte),
        Cmp(Reg.RAX, 0),
        DirCondJump(loopLabel, Flag.Greater),

        Cmp(Reg.RSI, 0),
        DirCondJump(noMinusLabel, Flag.GreaterOrEqual),
        Sub(Reg.RBX, 1),
        StoreImm(Address(Reg.RBX), 45, RegSize.Byte),

        Label(noMinusLabel),
        Sub(Reg.RSP, 48),
        LoadImm(Reg.RCX, -11),
        IndCall(Address("GetStdHandle")),
        Mov(Reg.RCX, Reg.RAX),
        Mov(Reg.RDX, Reg.RBX),
        Lea(Reg.R8, Address(bufferLabel) + 32),
        Sub(Reg.R8, Reg.RBX),
        Lea(Reg.R9, Reg.RSP + 32),
        StoreImm(Reg.RSP + 32, 0),
        IndCall(Address("WriteFile")),
        Add(Reg.RSP, 48),
        Ret()
      )
    }
  )))
)

def analyzeFile(stmts: List[syn.GlobalStmt], file: File): Module = {
  lazy val module: Module = new Module(file, {
    val (varInits, userVars) = (for (stmt <- stmts) yield {
      lazy val (analyzedPattern: Pattern[UserGlobalVar], vars: List[UserGlobalVar]) = Pattern.map((pattern, patternNav) => new UserGlobalVar(module, pattern.name, init, patternNav, pattern.datatype, pattern.range), stmt.pattern)
      lazy val init = new UserGlobalVarInit(module, stmt.expr, analyzedPattern)
      (init, vars)
    }).unzip

    ((userVars.flatten ::: builtinVars(module)).groupBy(_.name), varInits)
  })

  module.varInits.foreach(_.typeCheck())

//  val secondaryStack = AsmGen.bss(1024 * 256, 16)
//
//  val ctx = new ExprCodeGenContext()
//
//  def iterateUserGlobalVars(pattern: Pattern[UserGlobalVar], offset: Int): Unit = pattern match {
//    case VarPattern(patternVar, _) => patternVar.datatype.generateCopyCode(ctx, Address(patternVar.label.get), Reg.RBP + offset)
//    case TuplePattern(elements, _) => elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3.map(_ + offset)).foreach(iterateUserGlobalVars.tupled)
//  }
//
//  for {
//    varInit <- module.varInits
//    if varInit.analyzedExpr.returnType.runtime
//  } {
//    ctx.secondaryOffset = 0
//    varInit.analyzedExpr.generateCode(ctx)
//    iterateUserGlobalVars(varInit.analyzedPattern, 0)
//  }
//
//  val initFunction = AsmGen.functionLabel()
//  AsmGen.function(initFunction, ctx.code ::: List(Ret()))
//
//  AsmGen.main(
//    Lea(Reg.RBP, Address(secondaryStack)),
//    DirCall(initFunction),
//    DirCall(module.vars("main").head.constVal.get.asInstanceOf[ConstFunction].function.label),
//    Sub(Reg.RSP, 56),
//    Xor(Reg.RCX, Reg.RCX),
//    IndCall(Address(AsmGen.windowsFunction("ExitProcess")))
//  )

  module
}
