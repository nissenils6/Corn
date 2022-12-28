package sem

import core.*
import gen.*
import opt.{Ctrl, Data}

import scala.collection.mutable

type BinGraphOp = (opt.Data, opt.Data) => opt.OpNext
type CompGraphOp = opt.Data => opt.OpNext

def binaryOperatorGraph(graphOp: BinGraphOp, params: List[opt.Datatype], returnType: opt.Datatype, index: Int = 0): Option[opt.Fun] = {
  val op = graphOp(Data(None, 0), Data(None, 1))
  val retOp = opt.Ret(List(Data(Some(op), index)))
  op.next = retOp

  Some(new opt.Fun(op, opt.FunDatatype(params, List(returnType)), List()))
}

def compOperatorGraph(graphOp: CompGraphOp, swapped: Boolean, params: List[opt.Datatype], returnType: opt.Datatype): Option[opt.Fun] = {
  val subOp = opt.AddInt(List(Data(None, if swapped then 1 else 0)), List(Data(None, if swapped then 0 else 1)))
  val op = graphOp(Data(Some(subOp), 0))
  val retOp = opt.Ret(List(Data(Some(op), 0)))
  subOp.next = op
  op.next = retOp

  Some(new opt.Fun(subOp, opt.FunDatatype(params, List(returnType)), List()))
}

def simpleOperator(module: => Module, name: String, compileTimeFunction: (Long, Long) => Long, graphOp: BinGraphOp): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, name, ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), IntDatatype(false),
    Some(args => ConstInt(compileTimeFunction(args.head.toInt, args(1).toInt))),
    binaryOperatorGraph(graphOp, List(opt.IntDatatype, opt.IntDatatype), opt.IntDatatype)
  )))

def mulOperator(module: => Module): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, "*", ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), IntDatatype(false),
    Some(args => ConstInt(args.head.toInt * args(1).toInt)),
    binaryOperatorGraph((data1, data2) => opt.MultInt(List(data1, data2)), List(opt.IntDatatype, opt.IntDatatype), opt.IntDatatype)
  )))

def divOperator(module: => Module, name: String, compileTimeFunction: (Long, Long) => Long, index: Int): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, name, ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), IntDatatype(false),
    Some(args => ConstInt(compileTimeFunction(args.head.toInt, args(1).toInt))),
    binaryOperatorGraph(opt.DivInt.apply, List(opt.IntDatatype, opt.IntDatatype), opt.IntDatatype, index)
  )))

def comparisonOperator(module: => Module, name: String, compileTimeFunction: (Long, Long) => Boolean, swapped: Boolean, graphOp: CompGraphOp): BuiltinGlobalVar =
  new BuiltinGlobalVar(module, name, ConstFunction(new BuiltinFun(module, List(IntDatatype(false), IntDatatype(false)), BoolDatatype(false),
    Some(args => ConstBool(compileTimeFunction(args.head.toInt, args(1).toInt))),
    compOperatorGraph(graphOp, swapped, List(opt.IntDatatype, opt.IntDatatype), opt.BoolDatatype)
  )))

def builtinVars(module: => Module): List[GlobalVar] = List(
  new BuiltinGlobalVar(module, "Int", ConstType(IntDatatype(false))),
  new BuiltinGlobalVar(module, "Bool", ConstType(BoolDatatype(false))),
  new BuiltinGlobalVar(module, "Type", ConstType(TypeDatatype(false))),

  simpleOperator(module, "+", _ + _, (data1, data2) => opt.AddInt(List(data1, data2), List())),
  simpleOperator(module, "-", _ - _, (data1, data2) => opt.AddInt(List(data1), List(data2))),
  simpleOperator(module, "&", _ & _, (data1, data2) => opt.BitwiseInt(opt.BitwiseOp.And, List(data1, data2))),
  simpleOperator(module, "|", _ | _, (data1, data2) => opt.BitwiseInt(opt.BitwiseOp.Or, List(data1, data2))),
  simpleOperator(module, "^", _ ^ _, (data1, data2) => opt.BitwiseInt(opt.BitwiseOp.Xor, List(data1, data2))),

  mulOperator(module),
  divOperator(module, "/", _ / _, 0),
  divOperator(module, "%", _ % _, 1),

  comparisonOperator(module, ">", _ > _, false, data => opt.CompInt(opt.CompType.Pos, data)),
  comparisonOperator(module, "<", _ < _, true, data => opt.CompInt(opt.CompType.Pos, data)),
  comparisonOperator(module, ">=", _ >= _, false, data => opt.CompInt(opt.CompType.PosOrZero, data)),
  comparisonOperator(module, "<=", _ <= _, true, data => opt.CompInt(opt.CompType.PosOrZero, data)),
  comparisonOperator(module, "==", _ == _, false, data => opt.CompInt(opt.CompType.Zero, data)),
  comparisonOperator(module, "!=", _ != _, false, data => opt.CompInt(opt.CompType.NotZero, data)),

  new BuiltinGlobalVar(module, "println", ConstFunction(new BuiltinFun(module, List(IntDatatype(false)), UnitDatatype(false),
    None,
    {
      val op = opt.Print(Data(None, 0))
      val retOp = opt.Ret(List(Data(op)))
      op.next = retOp

      Some(new opt.Fun(op, opt.FunDatatype(List(opt.IntDatatype), List(opt.UnitDatatype)), List()))
    }
  )))
)

def analyzeFile(stmts: List[syn.GlobalStmt], file: File): Module = {
  lazy val module: Module = new Module(file, {
    val (varInits, userVars) = (for (stmt <- stmts) yield {
      lazy val (analyzedPattern: Pattern[UserGlobalVar], vars: List[UserGlobalVar]) = Pattern.analyze((pattern, patternNav) => new UserGlobalVar(module, pattern.name, init, patternNav, pattern.datatype, pattern.range), stmt.pattern)
      lazy val init = new UserGlobalVarInit(module, stmt.expr, analyzedPattern)
      (init, vars)
    }).unzip

    ((userVars.flatten ::: builtinVars(module)).groupBy(_.name), varInits)
  })

  module.varInits.foreach(_.typeCheck())

  module
}
