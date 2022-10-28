import scala.collection.mutable

def analyzeFile(stmts: List[GlobalStmt], file: File): Module = {
  val module: Module = new Module(file)

  module.vars("Int") = new BuiltinGlobalVar(module, "Int", ConstType(IntDatatype))
  module.vars("Type") = new BuiltinGlobalVar(module, "Type", ConstType(TypeDatatype))

  for (stmt <- stmts) stmt match {
    case FunGlobalStmt(name, parameters, returnType, expr, range) =>
      if (!module.funTables.contains(name))
        module.funTables(name) = GlobalFunTable(module, name)
      val fun = GlobalFun(module, name, returnType, expr)
      module.funTables(name).funs.append(fun)
      fun.args = parameters.map(param => mapPattern((name, const, typeExpr) => {
        if (fun.params.contains(name)) throw Error.duplicate(name, range)
        val localVar = new LocalVar(fun.module, name, typeExpr)
        fun.params(name) = localVar
        localVar
      }, param))
      println(range.underline)
    case LetGlobalStmt(pattern, expr, range) =>
      lazy val mappedPattern = mapPattern((name, const, typeExpr) => {
        if (module.vars.contains(name)) throw Error.duplicate(name, range)
        val globalVar = new UserGlobalVar(module, name, init, const, typeExpr)
        module.vars(name) = globalVar
        globalVar
      }, pattern)
      lazy val init: UserGlobalVarInit = new UserGlobalVarInit(module, expr, mappedPattern)
      module.varInits.append(init)
  }

  module.varInits.foreach(_.typeCheck())
  module.funTables.foreach(_._2.funs.foreach(_.typeCheck()))

  println(module.format(0))

  module
}
