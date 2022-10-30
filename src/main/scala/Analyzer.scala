import Asm.Xor

import scala.collection.mutable

def analyzeFile(stmts: List[GlobalStmt], file: File): Module = {
  val module: Module = new Module(file)

  module.addVar(new BuiltinGlobalVar(module, "Int", ConstType(IntDatatype)))
  module.addVar(new BuiltinGlobalVar(module, "Type", ConstType(TypeDatatype)))

  module.addFun(new BuiltinGlobalFun(module, "+", List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt + args(1).toInt)),
    () => List(
      Load(Reg.RAX, Reg.RSP + 16),
      Asm.Add(Reg.RSP + 24, Reg.RAX),
      Ret()
    )
  ))

  module.addFun(new BuiltinGlobalFun(module, "-", List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt - args(1).toInt)),
    () => List(
      Load(Reg.RAX, Reg.RSP + 16),
      Asm.Sub(Reg.RSP + 24, Reg.RAX),
      Ret()
    )
  ))

  module.addFun(new BuiltinGlobalFun(module, "*", List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt * args(1).toInt)),
    () => List(
      Load(Reg.RAX, Reg.RSP + 24),
      Load(Reg.RCX, Reg.RSP + 16),
      Imul(Reg.RAX, Reg.RCX),
      Store(Reg.RSP + 16, Reg.RAX),
      Ret()
    )
  ))

  module.addFun(new BuiltinGlobalFun(module, "/", List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt / args(1).toInt)),
    () => List(
      Load(Reg.RAX, Reg.RSP + 24),
      Load(Reg.RCX, Reg.RSP + 16),
      Asm.Xor(Reg.RDX, Reg.RDX),
      Idiv(Reg.RCX),
      Store(Reg.RSP + 16, Reg.RAX),
      Ret()
    )
  ))

  module.addFun(new BuiltinGlobalFun(module, "%", List(IntDatatype, IntDatatype), IntDatatype,
    args => Some(ConstInt(args.head.toInt % args(1).toInt)),
    () => List(
      Load(Reg.RAX, Reg.RSP + 24),
      Load(Reg.RCX, Reg.RSP + 16),
      Asm.Xor(Reg.RDX, Reg.RDX),
      Idiv(Reg.RCX),
      Store(Reg.RSP + 16, Reg.RDX),
      Ret()
    )
  ))

  module.addFun(new BuiltinGlobalFun(module, "println", List(IntDatatype), UnitDatatype,
    args => None,
    () => {
      val noNegLabel = CodeGen.label()
      val loopLabel = CodeGen.label()
      val noMinusLabel = CodeGen.label()

      val (bufferLabel, bufferEndLabel) = CodeGen.bss(32)
      CodeGen.windowsFunction("WriteFile")
      CodeGen.windowsFunction("GetStdHandle")

      List(
        Load(Reg.RAX, Reg.RSP + 16),
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
  ))

  for (stmt <- stmts) stmt match {
    case FunGlobalStmt(name, parameters, returnType, expr, range) =>
      val fun = module.addFun(UserGlobalFun(module, name, returnType, expr, range))
      fun.args = parameters.map(param => mapPattern((name, const, typeExpr, range) => {
        if (fun.params.contains(name)) throw Error.duplicate("parameter", name, range, fun.params(name).range)
        val localVar = new LocalVar(fun.module, name, typeExpr, range)
        fun.params(name) = localVar
        localVar
      }, param))
    case LetGlobalStmt(pattern, expr, _) =>
      var init: UserGlobalVarInit = null
      val mappedPattern = mapPattern((name, const, typeExpr, range) => module.addVar(new UserGlobalVar(module, name, init, const, typeExpr, range)), pattern)
      init = new UserGlobalVarInit(module, expr, mappedPattern)
      module.varInits.append(init)
  }

  module.varInits.foreach(_.typeCheck())
  module.funTables.foreach(_._2.funs.foreach(_.typeCheck()))

  CodeGen.main(
    Asm.Sub(Reg.RSP, 8),
    DirCall(module.funTables("main").funs.head.label),
    Asm.Sub(Reg.RSP, 48),
    Asm.Xor(Reg.RCX, Reg.RCX),
    IndCall(Address(CodeGen.windowsFunction("ExitProcess")))
  )

  module
}
