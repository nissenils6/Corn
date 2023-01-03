package sem

import core.*
import syn.AssignExpr

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable
import scala.compiletime.ops.int
import scala.concurrent.ExecutionContext.global

abstract class Expr() {
  def module: Module
  def range: FilePosRange

  lazy val returnType: Datatype = this match {
    case CallExpr(function, _, _, _) => function.returnType.asInstanceOf[FunDatatype].returnType
    case GlobalVarExpr(globalVar, _, _) => globalVar.datatype
    case RefGlobalVarExpr(globalVar, _, _) => RefDatatype(globalVar.datatype, false)
    case LocalVarExpr(localVar, _, _) => localVar.datatype
    case RefLocalVarExpr(localVar, _, _) => RefDatatype(localVar.datatype, false)
    case ValExpr(expr, _, _) => expr.returnType.asInstanceOf[RefDatatype].datatype
    case IntExpr(_, _, _) => IntDatatype(false)
    case BoolExpr(_, _, _) => BoolDatatype(false)
    case TupleExpr(elements, _, _) => TupleDatatype(elements.map(_.returnType), false)
    case BlockExpr(_, lastExpr, _, _, _) => lastExpr.returnType
    case UnitExpr(_, _) => UnitDatatype(false)
    case LetExpr(_, expr, _, _) => UnitDatatype(false)
    case AssignExpr(_, expr, _, _) => UnitDatatype(false)
    case FunExpr(fun, _, _) => fun.signature
    case FunTypeExpr(_, _, _, _) => TypeDatatype(false)
    case RefTypeExpr(_, _, _) => TypeDatatype(false)
    case IfExpr(_, ifBlock, _, _, _) => ifBlock.returnType
    case MutExpr(_, _, _, _) => TypeDatatype(false)
  }

  def gatherFuns(funs: mutable.Set[Fun]): Unit = this match {
    case CallExpr(function, args, _, _) =>
      function.gatherFuns(funs)
      args.foreach(_.gatherFuns(funs))
    case ValExpr(expr, _, _) => expr.gatherFuns(funs)
    case TupleExpr(elements, _, _) =>
      elements.foreach(_.gatherFuns(funs))
    case BlockExpr(exprs, lastExpr, _, _, _) =>
      exprs.foreach(_.gatherFuns(funs))
      lastExpr.gatherFuns(funs)
    case LetExpr(_, expr, _, _) => expr.gatherFuns(funs)
    case AssignExpr(left, right, _, _) =>
      left.gatherFuns(funs)
      right.gatherFuns(funs)
    case FunExpr(fun, _, _) =>
      fun.gatherFuns(funs)
    case IfExpr(condition, ifBlock, elseBlock, _, _) =>
      condition.gatherFuns(funs)
      ifBlock.gatherFuns(funs)
      elseBlock.gatherFuns(funs)
    case _ => ()
  }

  lazy val gatherLocals: List[LocalVar] = this match {
    case CallExpr(function, args, _, _) => function.gatherLocals ::: args.flatMap(_.gatherLocals)
    case ValExpr(expr, _, _) => expr.gatherLocals
    case TupleExpr(elements, _, _) => elements.flatMap(_.gatherLocals)
    case BlockExpr(exprs, lastExpr, vars, _, _) => vars ::: lastExpr.gatherLocals ::: exprs.flatMap(_.gatherLocals)
    case LetExpr(_, expr, _, _) => expr.gatherLocals
    case AssignExpr(left, right, _, _) => left.gatherLocals ::: right.gatherLocals
    case IfExpr(condition, ifBlock, elseBlock, _, _) => condition.gatherLocals ::: ifBlock.gatherLocals ::: elseBlock.gatherLocals
    case _ => List.empty
  }

  lazy val runtime: Boolean = this match {
    case CallExpr(function, args, _, _) => function.runtime && args.forall(_.runtime)
    case GlobalVarExpr(globalVar, _, _) => globalVar.runtime
    case RefGlobalVarExpr(globalVar, _, _) => globalVar.runtime
    case LocalVarExpr(localVar, _, _) => localVar.runtime
    case RefLocalVarExpr(localVar, _, _) => localVar.runtime
    case ValExpr(expr, _, _) => expr.runtime
    case IntExpr(_, _, _) | BoolExpr(_, _, _) | UnitExpr(_, _) => true
    case TupleExpr(elements, _, _) => elements.forall(_.runtime)
    case BlockExpr(exprs, lastExpr, _, _, _) => lastExpr.runtime && exprs.forall(_.runtime)
    case LetExpr(_, expr, _, _) => expr.runtime
    case AssignExpr(_, expr, _, _) => expr.runtime
    case FunExpr(fun, _, _) => fun.runtime
    case FunTypeExpr(_, _, _, _) | RefTypeExpr(_, _, _) | MutExpr(_, _, _, _) => false
    case IfExpr(condition, ifBlock, elseBlock, _, _) => condition.runtime && ifBlock.runtime && elseBlock.runtime
  }

  lazy val compiletime: Boolean = this match {
    case CallExpr(function, args, _, _) => function.compiletime && args.forall(_.compiletime)
    case GlobalVarExpr(globalVar, _, _) => globalVar.compiletime
    case RefGlobalVarExpr(globalVar, _, _) => globalVar.compiletime
    case LocalVarExpr(localVar, _, _) => localVar.compiletime
    case RefLocalVarExpr(localVar, _, _) => localVar.compiletime
    case ValExpr(expr, _, _) => expr.compiletime
    case IntExpr(_, _, _) | BoolExpr(_, _, _) | UnitExpr(_, _) => true
    case TupleExpr(elements, _, _) => elements.forall(_.compiletime)
    case BlockExpr(exprs, lastExpr, _, _, _) => lastExpr.compiletime && exprs.forall(_.compiletime)
    case LetExpr(_, expr, _, _) => expr.compiletime
    case AssignExpr(_, expr, _, _) => expr.compiletime
    case FunExpr(fun, _, _) => fun.compiletime
    case FunTypeExpr(_, _, _, _) | RefTypeExpr(_, _, _) | MutExpr(_, _, _, _) => false
    case IfExpr(condition, ifBlock, elseBlock, _, _) => condition.compiletime && ifBlock.compiletime && elseBlock.compiletime
  }

  lazy val constVal: Option[ConstVal] = this match {
    case CallExpr(function, args, _, _) => for {
      fun <- function.constVal
      argValues <- args.map(_.constVal).extract
      returnValue <- fun.asInstanceOf[ConstFunction].function.constEval(argValues)
    } yield returnValue
    case GlobalVarExpr(globalVar, _, _) => globalVar.constVal
    case RefGlobalVarExpr(globalVar, _, _) => if globalVar.constVal.nonEmpty then Some(ConstRef(VarRefBox(globalVar))) else None
    case LocalVarExpr(localVar, _, _) => localVar.constVal
    case RefLocalVarExpr(localVar, _, _) => if localVar.constVal.nonEmpty then Some(ConstRef(VarRefBox(localVar))) else None
    case ValExpr(expr, _, _) => expr.constVal.flatMap { case ConstRef(VarRefBox(variable)) => variable.constVal }
    case IntExpr(int, _, _) => Some(ConstInt(int))
    case BoolExpr(bool, _, _) => Some(ConstBool(bool))
    case TupleExpr(elements, _, _) => elements.map(_.constVal).extract.map(vals => ConstTuple(vals))
    case UnitExpr(_, _) => Some(ConstUnit)
    case LetExpr(_, _, _, range) => None
    case AssignExpr(_, _, _, range) => None
    case FunExpr(fun, _, _) => Some(ConstFunction(fun))
    case FunTypeExpr(parameters, returnType, _, _) => Some(ConstType(FunDatatype(parameters.map(_.constDatatype), returnType.constDatatype, false)))
    case RefTypeExpr(expr, _, _) => Some(ConstType(RefDatatype(expr.constDatatype, false)))
    case IfExpr(condition, ifBlock, elseBlock, _, _) => condition.constVal.flatMap(evaluatedCondition => if evaluatedCondition.toBool then ifBlock.constVal else elseBlock.constVal)
    case MutExpr(expr, mutable, _, _) => for {
      constType <- expr.constVal
      datatype <- constType.toType
    } yield ConstType(datatype.withMut(mutable))
    case _ => constEval(ExprConstEvalContext(module))
  }

  lazy val constDatatype: Datatype = constVal.flatMap(_.toType) match {
    case Some(datatype) => datatype
    case None => throw Error.datatypeExpected(range)
  }

  def constEval(ctx: ExprConstEvalContext): Option[ConstVal] = this match {
    case CallExpr(function, args, _, _) => for {
      fun <- function.constEval(ctx)
      argValues <- args.map(_.constEval(ctx)).extract
      returnValue <- fun.asInstanceOf[ConstFunction].function.constEval(argValues)
    } yield returnValue
    case GlobalVarExpr(globalVar, _, _) => globalVar.constVal
    case RefGlobalVarExpr(globalVar, _, _) => if globalVar.constVal.nonEmpty then Some(ConstRef(VarRefBox(globalVar))) else None
    case LocalVarExpr(localVar, _, _) => ctx.lookup(localVar)
    case RefLocalVarExpr(localVar, _, _) => Some(ConstRef(VarRefBox(localVar)))
    case ValExpr(expr, _, _) => expr.constEval(ctx).flatMap {
      case ConstRef(VarRefBox(localVar: LocalVar)) => ctx.lookup(localVar)
      case ConstRef(VarRefBox(globalVar: GlobalVar)) => globalVar.constVal
    }
    case IntExpr(int, _, _) => Some(ConstInt(int))
    case BoolExpr(bool, _, _) => Some(ConstBool(bool))
    case TupleExpr(elements, _, _) => elements.map(element => element.constEval(ctx)).extract.map(vals => ConstTuple(vals))
    case BlockExpr(exprs, lastExpr, _, _, _) => ctx.scope(if exprs.map(_.constEval(ctx)).forall(_.nonEmpty) then lastExpr.constEval(ctx) else None)
    case UnitExpr(_, _) => Some(ConstUnit)
    case LetExpr(pattern, expr, _, _) => expr.constEval(ctx).map(value => ctx.add(pattern, value))
    case AssignExpr(GlobalVarExpr(globalVar, _, range), expr, _, _) => throw Error.internal(range)
    case AssignExpr(LocalVarExpr(localVar, _, range), expr, _, _) => for {
      value <- expr.constEval(ctx)
    } yield ctx.add(localVar, value)
    case AssignExpr(ValExpr(ptrExpr, _, range), expr, _, _) => for {
      constRef <- ptrExpr.constEval(ctx)
      value <- expr.constEval(ctx)
    } yield constRef match {
      case ConstRef(VarRefBox(localVar: LocalVar)) => ctx.add(localVar, value)
      case ConstRef(VarRefBox(globalVar: GlobalVar)) => throw Error.internal(range)
    }
    case FunExpr(fun, _, _) => Some(ConstFunction(fun))
    case FunTypeExpr(parameters, returnType, _, _) => Some(ConstType(FunDatatype(parameters.map(_.constDatatype), returnType.constDatatype, false)))
    case RefTypeExpr(expr, _, _) => Some(ConstType(RefDatatype(expr.constDatatype, false)))
    case IfExpr(condition, ifBlock, elseBlock, _, _) => condition.constEval(ctx).flatMap(evaluatedCondition => if evaluatedCondition.toBool then ifBlock.constEval(ctx) else elseBlock.constEval(ctx))
    case MutExpr(expr, mutable, _, _) => for {
      constType <- expr.constEval(ctx)
      datatype <- constType.toType
    } yield ConstType(datatype.withMut(mutable))
  }

  def generateIr(context: IrGenContext, localVars: Map[LocalVar, Int]): (opt.Op, opt.OpNext) = this match {
    case CallExpr(function, args, _, _) =>
      val (firstFunctionOp, lastFunctionOp) = function.generateIr(context, localVars)
      val argOps = args.map(_.generateIr(context, localVars))
      val callOp = opt.Call(Right(opt.Data(lastFunctionOp)), argOps.map(_._2).map(opt.Data.apply))
      val firstArgOp = opt.linkOpSections(callOp)(argOps)
      lastFunctionOp.next = firstArgOp
      (firstFunctionOp, callOp)
    case GlobalVarExpr(globalVar, _, _) =>
      val (optVar: opt.Var, optVarIdx: Int) = context(globalVar)
      opt.toPair(opt.ReadGlobal(optVar, optVarIdx))
    case RefGlobalVarExpr(globalVar, _, _) =>
      val (optVar: opt.Var, optVarIdx: Int) = context(globalVar)
      opt.toPair(opt.RefGlobal(optVar, optVarIdx))
    case LocalVarExpr(localVar, _, _) => opt.toPair(opt.ReadLocal(localVars(localVar)))
    case RefLocalVarExpr(localVar, _, _) => opt.toPair(opt.RefLocal(localVars(localVar)))
    case ValExpr(expr, _, _) =>
      val (firstExprOp, lastExprOp) = expr.generateIr(context, localVars)
      val readOp = opt.ReadRef(opt.Data(lastExprOp))
      lastExprOp.next = readOp
      (firstExprOp, readOp)
    case IntExpr(int, _, _) => opt.toPair(opt.IntLit(int))
    case BoolExpr(bool, _, _) => opt.toPair(opt.BoolLit(bool))
    case TupleExpr(elements, _, _) =>
      val elementOps = elements.map(_.generateIr(context, localVars))
      val tupleOp = opt.TupleLit(elementOps.map(_._2).map(opt.Data.apply))
      val firstElementOp = opt.linkOpSections(tupleOp)(elementOps)
      (firstElementOp, tupleOp)
    case BlockExpr(exprs, lastExpr, _, _, _) =>
      val exprOps = exprs.map(_.generateIr(context, localVars))
      val (firstRetExprOp, lastRetExprOp) = lastExpr.generateIr(context, localVars)
      val firstExprOp = opt.linkOpSections(firstRetExprOp)(exprOps)
      (firstExprOp, lastRetExprOp)
    case UnitExpr(_, _) => opt.toPair(opt.UnitLit())
    case LetExpr(pattern, expr, _, _) =>
      val (firstExprOp, lastExprOp) = expr.generateIr(context, localVars)
      val patternOps = Pattern.generateIrLocal(pattern, opt.Data(lastExprOp), localVars)
      val unitOp = opt.UnitLit()
      val firstPatternOp = opt.linkOps(unitOp)(patternOps)

      lastExprOp.next = firstPatternOp

      (firstExprOp, unitOp)
    case AssignExpr(LocalVarExpr(localVar, _, _), expr, _, _) =>
      val (firstExprOp, lastExprOp) = expr.generateIr(context, localVars)
      val writeOp = opt.WriteLocal(localVars(localVar), opt.Data(lastExprOp))

      lastExprOp.next = writeOp

      (firstExprOp, writeOp)
    case AssignExpr(GlobalVarExpr(globalVar, _, _), expr, _, _) =>
      val (firstExprOp, lastExprOp) = expr.generateIr(context, localVars)
      val (optVar: opt.Var, optVarIdx: Int) = context(globalVar)
      val writeOp = opt.WriteGlobal(optVar, optVarIdx, opt.Data(lastExprOp))

      lastExprOp.next = writeOp

      (firstExprOp, writeOp)
    case AssignExpr(ValExpr(refExpr, _, _), expr, _, _) =>
      val (firstRefExprOp, lastRefExprOp) = refExpr.generateIr(context, localVars)
      val (firstExprOp, lastExprOp) = expr.generateIr(context, localVars)
      val writeOp = opt.WriteRef(opt.Data(lastRefExprOp), opt.Data(lastExprOp))

      lastRefExprOp.next = firstExprOp
      lastExprOp.next = writeOp

      (firstRefExprOp, writeOp)
    case FunExpr(fun, _, _) => opt.toPair(opt.FunLit(context(fun)))
    case IfExpr(condition, ifBlock, elseBlock, _, _) =>
      val (firstConditionOp, lastConditionOp) = condition.generateIr(context, localVars)

      val (firstIfOp, lastIfOp) = ifBlock.generateIr(context, localVars)
      val ifBlockNode = opt.Block(firstIfOp)

      val (firstElseOp, lastElseOp) = elseBlock.generateIr(context, localVars)
      val elseBlockNode = opt.Block(firstElseOp)

      val ifOp = opt.If(opt.Data(lastConditionOp), ifBlockNode, elseBlockNode, List(ifBlock.returnType.optDatatype))
      val endIfNode = opt.EndIf(ifOp, List(opt.Data(lastIfOp)))
      val endElseNode = opt.EndIf(ifOp, List(opt.Data(lastElseOp)))

      lastConditionOp.next = ifOp
      lastIfOp.next = endIfNode
      lastElseOp.next = endElseNode

      (firstConditionOp, ifOp)
    case expr: Expr => throw Error.internal(s"Attempt to generate ir from compile time expression (this is bug in the semantic analyzer)", expr.range)
  }

  def format(indentation: Int): String = this match {
    case CallExpr(function, args, _, _) => s"${function.format(indentation)}(${args.map(_.format(indentation)).mkString(", ")})"
    case GlobalVarExpr(globalVar, _, _) => s"${globalVar.name}"
    case RefGlobalVarExpr(globalVar, _, _) => s"ref ${globalVar.name}"
    case LocalVarExpr(localVar, _, _) => s"${localVar.name}"
    case RefLocalVarExpr(localVar, _, _) => s"ref ${localVar.name}"
    case ValExpr(expr, _, _) => s"val ${expr.format(indentation)}"
    case IntExpr(int, _, _) => s"$int"
    case BoolExpr(bool, _, _) => s"$bool"
    case TupleExpr(elements, _, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case BlockExpr(exprs, lastExpr, _, _, _) => s"{\n${exprs.map(e => s"${" " * (indentation + 1)}${e.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
    case UnitExpr(_, _) => s"()"
    case LetExpr(pattern, expr, _, _) => s"let ${pattern.format(indentation)} = ${expr.format(indentation)}"
    case AssignExpr(left, right, _, _) => s"${left.format(indentation)} = ${right.format(indentation)}"
    case FunExpr(fun, _, _) => s"${fun.format(indentation)}"
    case FunTypeExpr(parameters, returnType, _, _) => s"(${parameters.map(_.format(indentation)).mkString(", ")}) => ${returnType.format(indentation)}"
    case RefTypeExpr(expr, _, _) => s"ref ${expr.format(indentation)}"
    case IfExpr(condition, ifBlock, elseBlock, _, _) => s"if ${condition.format(indentation)} then ${ifBlock.format(indentation)} else ${elseBlock.format(indentation)}"
    case MutExpr(expr, mutable, _, _) => s"${if mutable then "mut" else "const"} ${expr.format(indentation)}"
  }
}

case class CallExpr(function: Expr, args: List[Expr], module: Module, range: FilePosRange) extends Expr
case class GlobalVarExpr(globalVar: GlobalVar, module: Module, range: FilePosRange) extends Expr
case class RefGlobalVarExpr(globalVar: GlobalVar, module: Module, range: FilePosRange) extends Expr
case class LocalVarExpr(localVar: LocalVar, module: Module, range: FilePosRange) extends Expr
case class RefLocalVarExpr(localVar: LocalVar, module: Module, range: FilePosRange) extends Expr
case class ValExpr(expr: Expr, module: Module, range: FilePosRange) extends Expr
case class IntExpr(int: Long, module: Module, range: FilePosRange) extends Expr
case class BoolExpr(bool: Boolean, module: Module, range: FilePosRange) extends Expr
case class TupleExpr(elements: List[Expr], module: Module, range: FilePosRange) extends Expr
case class BlockExpr(exprs: List[Expr], lastExpr: Expr, vars: List[LocalVar], module: Module, range: FilePosRange) extends Expr
case class UnitExpr(module: Module, range: FilePosRange) extends Expr
case class LetExpr(pattern: Pattern[LocalVar], expr: Expr, module: Module, range: FilePosRange) extends Expr
case class AssignExpr(left: Expr, right: Expr, module: Module, range: FilePosRange) extends Expr
case class FunExpr(fun: Fun, module: Module, range: FilePosRange) extends Expr
case class FunTypeExpr(parameters: List[Expr], returnTypeExpr: Expr, module: Module, range: FilePosRange) extends Expr
case class RefTypeExpr(expr: Expr, module: Module, range: FilePosRange) extends Expr
case class IfExpr(condition: Expr, ifBlock: Expr, elseBlock: Expr, module: Module, range: FilePosRange) extends Expr
case class MutExpr(expr: Expr, mutable: Boolean, module: Module, range: FilePosRange) extends Expr

class LocalVar(val module: Module, val name: String, letExpr: => Option[LetExpr], patternNav: PatternNav, typeExpr: Option[syn.Expr], val range: FilePosRange) extends Var {
  lazy val datatype: Datatype = typeExpr.map(typeExpr => analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype).getOrElse(patternNav.datatype(letExpr match {
    case Some(letExpr) => letExpr.expr.returnType.withMut(false)
    case None => throw Error.internal(module.file)
  }))
  lazy val constVal: Option[ConstVal] = if datatype.mutable then None else letExpr.flatMap(_.constVal.map(patternNav.const))

  lazy val runtime: Boolean = datatype.runtime && letExpr.forall(_.runtime)

  lazy val compiletime: Boolean = letExpr.exists(_.compiletime)
}

def overload(vars: List[Var], funRange: FilePosRange, args: List[Expr]): Option[(Var, List[Expr])] = {
  def fit(variable: Var): Option[(Var, List[Expr])] = variable.datatype match {
    case funDatatype: FunDatatype if args.map(_.returnType).zip(funDatatype.params).forall(_ ~=> _) => Some((variable, args))
    case _ => None
  }

  vars.map(fit).filter(_.nonEmpty).map(_.get) match {
    case List() => None
    case List(single) => Some(single)
    case varList => throw Error(Error.SEMANTIC, funRange.file, ErrorComponent(funRange, Some("Multiple matches for overloaded reference")) :: varList.map { (globalVar, _) => ErrorComponent(globalVar.range, Some("One match")) }, Some("Ambiguous reference"))
  }
}

class ExprParsingContext(val module: Module, val fun: Option[UserFun]) {
  private var vars = List[LocalVar]()

  def lookup(name: String, range: FilePosRange): Option[Var] = {
    @tailrec
    def rec(list: List[Var]): Option[Var] = if (list.isEmpty) {
      fun.flatMap(_.params.get(name)).orElse(module.vars.get(name).map {
        case List(globalVar) => globalVar
        case varList => throw Error(Error.SEMANTIC, range.file, ErrorComponent(range, Some("Multiple matches for reference")) :: varList.map(globalVar => ErrorComponent(globalVar.range, Some("One match"))), Some("Ambiguous reference"))
      })
    } else if (list.head.name == name) {
      Some(list.head)
    } else {
      rec(list.tail)
    }

    rec(vars)
  }

  def lookupOverload(name: String): List[List[Var]] = {
    def rec(list: List[LocalVar]): List[List[Var]] = if (list.isEmpty) {
      fun.flatMap(_.params.get(name)).map(localVar => List(localVar)).toList ::: module.vars.get(name).toList
    } else if (list.head.name == name) {
      List(list.head) :: rec(list.tail)
    } else {
      rec(list.tail)
    }

    rec(vars)
  }

  def add(localVars: List[LocalVar]): Unit = localVars.foreach(add)

  def add(localVar: LocalVar): LocalVar = {
    vars = localVar :: vars
    localVar
  }

  def scope[T](f: => T): (T, List[LocalVar]) = {
    val length = vars.length
    val value = f
    val (newVars, varsToDrop) = vars.splitAt(length)
    vars = newVars
    (value, varsToDrop)
  }

  override def toString: String = vars.map(_.name).mkString(", ")
}

class ExprConstEvalContext(val module: Module) {
  private var vars = mutable.Map[LocalVar, ConstVal]()

  def lookup(localVar: LocalVar): Option[ConstVal] = localVar.constVal.orElse(vars.get(localVar))

  def add(localVar: LocalVar, value: ConstVal): ConstVal = {
    vars(localVar) = value
    value
  }

  def add(pattern: Pattern[LocalVar], value: ConstVal): ConstVal = {
    pattern match {
      case VarPattern(patternVar, _) => add(patternVar, value)
      case TuplePattern(elements, _) => elements.zip(value.asInstanceOf[ConstTuple].elements).foreach(t => add(t._1, t._2))
    }
    value
  }

  def scope[T](f: => T): T = {
    val copy = vars.clone()
    val value = f
    vars = copy
    value
  }
}

def analyzeExpr(ctx: ExprParsingContext)(expr: syn.Expr): Expr = expr match {
  case syn.CallExpr(syn.IdenExpr(iden, funRange), posArgs, range) =>
    val overloadLayers = ctx.lookupOverload(iden)
    val analyzedPosArgs = posArgs.map(analyzeExpr(ctx))
    val appliedLayers = overloadLayers.map(overload(_, funRange, analyzedPosArgs))
    appliedLayers.find(_.nonEmpty).flatten match {
      case Some((globalVar: GlobalVar, args)) => CallExpr(GlobalVarExpr(globalVar, ctx.module, funRange), args, ctx.module, range)
      case Some((localVar: LocalVar, args)) => CallExpr(LocalVarExpr(localVar, ctx.module, funRange), args, ctx.module, range)
      case _ => throw Error.internal(funRange)
    }
  case syn.CallExpr(function, posArgs, range) =>
    val analyzedFunExpr = analyzeExpr(ctx)(function)
    analyzedFunExpr.returnType match {
      case FunDatatype(params, _, _) =>
        if (params.length != posArgs.length) throw Error.internal(range.file)
        val analyzedArgs = posArgs.map(analyzeExpr(ctx))
        if (!params.zip(analyzedArgs.map(_.returnType)).forall(t => t._1 == t._2)) throw Error.internal(range)
        CallExpr(analyzedFunExpr, analyzedArgs, ctx.module, range)
      case datatype => throw Error.semantic(s"'$datatype' is not callable", function.range)
    }
  case syn.IdenExpr(iden, range) => ctx.lookup(iden, range) match {
    case Some(globalVar: GlobalVar) => GlobalVarExpr(globalVar, ctx.module, range)
    case Some(localVar: LocalVar) => LocalVarExpr(localVar, ctx.module, range)
    case None => throw Error.internal(ctx.toString, range)
  }
  case syn.RefExpr(syn.IdenExpr(iden, idenRange), range) => ctx.lookup(iden, idenRange) match {
    case Some(globalVar: GlobalVar) => RefGlobalVarExpr(globalVar, ctx.module, range | idenRange)
    case Some(localVar: LocalVar) => RefLocalVarExpr(localVar, ctx.module, range | idenRange)
    case None => throw Error.internal(ctx.toString, range | idenRange)
  }
  case syn.RefExpr(expr, range) => RefTypeExpr(analyzeExpr(ctx)(expr), ctx.module, range)
  case syn.ValExpr(expr, range) =>
    val analyzedExpr = analyzeExpr(ctx)(expr)
    if (!analyzedExpr.returnType.isInstanceOf[RefDatatype]) throw Error.internal(ctx.module.file)
    ValExpr(analyzedExpr, ctx.module, range)
  case syn.IntExpr(int, range) => IntExpr(int, ctx.module, range)
  case syn.BoolExpr(bool, range) => BoolExpr(bool, ctx.module, range)
  case syn.TupleExpr(elements, range) => TupleExpr(elements.map(analyzeExpr(ctx)), ctx.module, range)
  case syn.BlockExpr(exprs, lastExpr, range) =>
    val ((analyzedExprs, analyzedLastExpr), vars) = ctx.scope((exprs.map(analyzeExpr(ctx)), analyzeExpr(ctx)(lastExpr)))
    BlockExpr(analyzedExprs, analyzedLastExpr, vars, ctx.module, range)
  case syn.UnitExpr(range) => UnitExpr(ctx.module, range)
  case syn.LetExpr(pattern, expr, range) =>
    lazy val analyzedExpr = analyzeExpr(ctx)(expr)
    lazy val (analyzedPattern: Pattern[LocalVar], vars: List[LocalVar]) = Pattern.analyze((pattern, patternNav) => new LocalVar(ctx.module, pattern.name, Some(letExpr), patternNav, pattern.datatype, pattern.range), pattern)
    if (analyzedExpr.returnType !~=> analyzedPattern.datatype) throw Error.typeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
    lazy val letExpr = LetExpr(analyzedPattern, analyzedExpr, ctx.module, range)
    ctx.add(vars)
    letExpr
  case syn.AssignExpr(left, right, range) =>
    val analyzedLeft = analyzeExpr(ctx)(left)
    val analyzedRight = analyzeExpr(ctx)(right)
    if (analyzedRight.returnType !~=> analyzedLeft.returnType) throw Error.typeMismatch(analyzedRight.returnType, analyzedLeft.returnType, analyzedRight.range, analyzedLeft.range)
    if (!analyzedLeft.returnType.mutable) throw Error.internal(analyzedLeft.range)
    analyzedLeft match {
      case _: (LocalVarExpr | GlobalVarExpr | ValExpr) => ()
      case _ => throw Error.internal(analyzedLeft.range)
    }
    AssignExpr(analyzedLeft, analyzedRight, ctx.module, range)
  case syn.FunExpr(parameters, returnType, expr, range) =>
    val fun = new UserFun(ctx.module, parameters, returnType, expr, range)
    FunExpr(fun, ctx.module, range)
  case syn.FunctionTypeExpr(parameters, returnType, range) => FunTypeExpr(parameters.map(analyzeExpr(ctx)), analyzeExpr(ctx)(returnType), ctx.module, range)
  case syn.IfExpr(condition, ifBlock, elseBlock, range) =>
    val analyzedCondition = analyzeExpr(ctx)(condition)
    if (analyzedCondition.returnType !~=> BoolDatatype(false)) throw Error(Error.SEMANTIC, range.file, List(ErrorComponent(analyzedCondition.range, Some(s"Expected 'Bool', found '${analyzedCondition.returnType}'"))))
    val analyzedIfBlock = analyzeExpr(ctx)(ifBlock)
    val analyzedElseBlock = analyzeExpr(ctx)(elseBlock)
    if (analyzedElseBlock.returnType !~=> analyzedIfBlock.returnType) throw Error(Error.SEMANTIC, range.file, List(
      ErrorComponent(analyzedIfBlock.range, Some(s"The if branch returns value of type '${analyzedIfBlock.returnType}'")),
      ErrorComponent(analyzedElseBlock.range, Some(s"The else branch returns value of type '${analyzedElseBlock.returnType}'"))
    ), Some("Both branches of an if statement must return values of the same type"))
    IfExpr(analyzedCondition, analyzedIfBlock, analyzedElseBlock, ctx.module, range)
  case syn.MutExpr(expr, mutable, range, kwRange) =>
    val analyzedExpr = analyzeExpr(ctx)(expr)
    analyzedExpr.constDatatype
    MutExpr(analyzedExpr, mutable, ctx.module, range)
}
