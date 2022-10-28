import scala.collection.mutable

def analyzeFile(stmts: List[GlobalStmt], file: File): Module = {
  val module: Module = new Module(file)

  module.addVar(new BuiltinGlobalVar(module, "Int", ConstType(IntDatatype)))
  module.addVar(new BuiltinGlobalVar(module, "Type", ConstType(TypeDatatype)))

  module.addFun(new BuiltinGlobalFun(module, "+", List(IntDatatype, IntDatatype), IntDatatype, args => Some(ConstInt(args.head.toInt + args(1).toInt))))
  module.addFun(new BuiltinGlobalFun(module, "-", List(IntDatatype, IntDatatype), IntDatatype, args => Some(ConstInt(args.head.toInt - args(1).toInt))))
  module.addFun(new BuiltinGlobalFun(module, "*", List(IntDatatype, IntDatatype), IntDatatype, args => Some(ConstInt(args.head.toInt * args(1).toInt))))
  module.addFun(new BuiltinGlobalFun(module, "/", List(IntDatatype, IntDatatype), IntDatatype, args => Some(ConstInt(args.head.toInt / args(1).toInt))))
  module.addFun(new BuiltinGlobalFun(module, "%", List(IntDatatype, IntDatatype), IntDatatype, args => Some(ConstInt(args.head.toInt % args(1).toInt))))

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

  module
}
