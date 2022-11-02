import Asm.Xor

import scala.collection.mutable

def builtinVars(module: => Module): List[GlobalVar] = List(
  new BuiltinGlobalVar(module, "Int", ConstType(IntDatatype)),
  new BuiltinGlobalVar(module, "Type", ConstType(TypeDatatype)),
  new BuiltinGlobalVar(module, "+", ConstFunction(new BuiltinFun(module, List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt + args(1).toInt)),
    () => List(
      Load(Reg.RAX, Reg.RBP + 8),
      Asm.Add(Address(Reg.RBP), Reg.RAX),
      Ret()
    )
  ))),
  new BuiltinGlobalVar(module, "-", ConstFunction(new BuiltinFun(module, List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt - args(1).toInt)),
    () => List(
      Load(Reg.RAX, Reg.RBP + 8),
      Asm.Sub(Address(Reg.RBP), Reg.RAX),
      Ret()
    )
  ))),
  new BuiltinGlobalVar(module, "*", ConstFunction(new BuiltinFun(module, List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt * args(1).toInt)),
    () => List(
      Load(Reg.RAX, Address(Reg.RBP)),
      Load(Reg.RCX, Reg.RBP + 8),
      Imul(Reg.RAX, Reg.RCX),
      Store(Address(Reg.RBP), Reg.RAX),
      Ret()
    )
  ))),
  new BuiltinGlobalVar(module, "/", ConstFunction(new BuiltinFun(module, List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt / args(1).toInt)),
    () => List(
      Load(Reg.RAX, Address(Reg.RBP)),
      Load(Reg.RCX, Reg.RBP + 8),
      Asm.Xor(Reg.RDX, Reg.RDX),
      Idiv(Reg.RCX),
      Store(Address(Reg.RBP), Reg.RAX),
      Ret()
    )
  ))),
  new BuiltinGlobalVar(module, "%", ConstFunction(new BuiltinFun(module, List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt % args(1).toInt)),
    () => List(
      Load(Reg.RAX, Address(Reg.RBP)),
      Load(Reg.RCX, Reg.RBP + 8),
      Asm.Xor(Reg.RDX, Reg.RDX),
      Idiv(Reg.RCX),
      Store(Address(Reg.RBP), Reg.RDX),
      Ret()
    )
  ))),
  new BuiltinGlobalVar(module, "println", ConstFunction(new BuiltinFun(module, List(IntDatatype), UnitDatatype,
    args => None,
    () => {
      val noNegLabel = CodeGen.label()
      val loopLabel = CodeGen.label()
      val noMinusLabel = CodeGen.label()

      val (_, bufferEndLabel) = CodeGen.bss(32)
      CodeGen.windowsFunction("WriteFile")
      CodeGen.windowsFunction("GetStdHandle")

      List(
        Load(Reg.RAX, Address(Reg.RBP)),
        Lea(Reg.RBX, Address(bufferEndLabel)),
        Asm.Sub(Reg.RBX, 1),
        StoreImm(Address(Reg.RBX), 10, RegSize.Byte),

        Asm.Cmp(Reg.RAX, 0),
        DirCondJump(noNegLabel, Flag.GreaterOrEqual),
        Neg(Reg.RAX),

        Mov(Reg.RSI, Reg.RAX, Some(noNegLabel)),
        LoadImm(Reg.RCX, 10),

        Asm.Xor(Reg.RDX, Reg.RDX, Some(loopLabel)),
        Idiv(Reg.RCX),
        Asm.Add(Reg.RDX, 48),
        Asm.Sub(Reg.RBX, 1),
        Store(Address(Reg.RBX), Reg.RDX, RegSize.Byte),
        Asm.Cmp(Reg.RAX, 0),
        DirCondJump(loopLabel, Flag.Greater),

        Asm.Cmp(Reg.RSI, 0),
        DirCondJump(noMinusLabel, Flag.GreaterOrEqual),
        Asm.Sub(Reg.RBX, 1),
        StoreImm(Address(Reg.RBX), 45, RegSize.Byte),

        Asm.Sub(Reg.RSP, 48, Some(noMinusLabel)),
        LoadImm(Reg.RCX, -11),
        IndCall(Address("GetStdHandle")),
        Mov(Reg.RCX, Reg.RAX),
        Mov(Reg.RDX, Reg.RBX),
        Lea(Reg.R8, Address(bufferEndLabel)),
        Asm.Sub(Reg.R8, Reg.RBX),
        Lea(Reg.R9, Reg.RSP + 32),
        StoreImm(Reg.RSP + 32, 0),
        IndCall(Address("WriteFile")),
        Asm.Add(Reg.RSP, 48),
        Ret()
      )
    }
  )))
)

def analyzeFile(stmts: List[GlobalStmt], file: File): Module = {
  lazy val module: Module = new Module(file, {
    val (analyzedPatterns, vars) = (for (stmt <- stmts) yield {
      lazy val (analyzedPattern: AnalyzedPattern[UserGlobalVar], vars: List[UserGlobalVar]) = mapPattern((pattern, patternNav) => new UserGlobalVar(module, pattern.name, init, patternNav, pattern.datatype, pattern.range), stmt.pattern)
      lazy val init = new UserGlobalVarInit(module, stmt.expr, analyzedPattern)
      (analyzedPattern, vars)
    }).unzip
    val vars2 = verifyPatterns(vars.flatten.asInstanceOf[List[GlobalVar]] ::: builtinVars(module))
    (vars2, analyzedPatterns, stmts.map(_.expr))
  })

  module.varInits.foreach(_.typeCheck())

  val (secondaryStack, _) = CodeGen.bss(1024 * 256, 16)

  CodeGen.main(
    Lea(Reg.RBP, Address(secondaryStack)),
    DirCall(module.vars("main").constVal.get.asInstanceOf[ConstFunction].function.label),
    Asm.Sub(Reg.RSP, 56),
    Asm.Xor(Reg.RCX, Reg.RCX),
    IndCall(Address(CodeGen.windowsFunction("ExitProcess")))
  )

  module
}
