package sem

import core.*
import gen.*
import opt.Controlflow
import syn.AssignExpr

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable

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
    case LetExpr(_, expr, _, _) => expr.returnType
    case AssignExpr(_, expr, _, _) => expr.returnType
    case FunExpr(fun, _, _) => fun.signature
    case FunTypeExpr(_, _, _, _) => TypeDatatype(false)
    case RefTypeExpr(_, _, _) => TypeDatatype(false)
    case IfExpr(_, ifBlock, _, _, _) => ifBlock.returnType
    case MutExpr(_, _, _, _) => TypeDatatype(false)
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
    case AssignExpr(GlobalVarExpr(globalVar, _, range), expr, _, _) => throw Error.todo(range)
    case AssignExpr(LocalVarExpr(localVar, _, range), expr, _, _) => for {
      value <- expr.constEval(ctx)
    } yield ctx.add(localVar, value)
    case AssignExpr(ValExpr(ptrExpr, _, range), expr, _, _) => for {
      constRef <- ptrExpr.constEval(ctx)
      value <- expr.constEval(ctx)
    } yield constRef match {
      case ConstRef(VarRefBox(localVar: LocalVar)) => ctx.add(localVar, value)
      case ConstRef(VarRefBox(globalVar: GlobalVar)) => throw Error.todo(range)
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

  def generateCode(ctx: ExprCodeGenContext): Unit = this match {
    case CallExpr(functionExpr, args, _, _) => functionExpr.constVal match {
      case Some(ConstFunction(function)) =>
        val argOffset = ctx.secondaryOffset
        args.foreach(_.generateCode(ctx))
        val afterArgOffset = ctx.secondaryOffset

        ctx.secondaryOffset = argOffset
        if (!function.generateInlineCode(ctx)) {
          ctx.secondaryOffset = afterArgOffset
          ctx.add(
            Sub(Reg.RSP, -(ctx.offset - 8)),
            Add(Reg.RBP, argOffset),
            DirCall(function.label),
            Sub(Reg.RBP, argOffset),
            Add(Reg.RSP, -(ctx.offset - 8))
          )
        }

        ctx.secondaryOffset = argOffset + function.returnType.size.roundUp(8)
      case _ =>
        val signature = functionExpr.returnType.asInstanceOf[FunDatatype]

        val argOffset = ctx.secondaryOffset
        functionExpr.generateCode(ctx)
        ctx.secondaryOffset -= 8
        ctx.add(
          Load(Reg.RAX, Reg.RBP + ctx.secondaryOffset),
          Store(Reg.RSP + (ctx.offset - 8), Reg.RAX)
        )

        ctx.offset -= 16
        args.foreach(_.generateCode(ctx))
        ctx.offset += 16

        ctx.add(
          Load(Reg.RAX, Reg.RSP + (ctx.offset - 8)),
          Sub(Reg.RSP, -(ctx.offset - 8)),
          Add(Reg.RBP, argOffset),
          IndRegCall(Reg.RAX),
          Sub(Reg.RBP, argOffset),
          Add(Reg.RSP, -(ctx.offset - 8))
        )
        ctx.secondaryOffset = argOffset + signature.returnType.size.roundUp(8)
    }
    case GlobalVarExpr(globalVar, _, range) =>
      val stackOffset = ctx.secondaryOffset
      ctx.secondaryOffset += globalVar.datatype.size.roundUp(8)
      globalVar.label match {
        case Some(label) => globalVar.datatype.generateCopyCode(ctx, Reg.RBP + stackOffset, Address(label))
        case None => {
          println()
          throw Error.todo(range)
        }
      }
    case RefGlobalVarExpr(globalVar, _, range) =>
      globalVar.label match {
        case Some(label) => ctx.add(
          Lea(Reg.RAX, Address(label)),
          Store(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
        )
        case None => throw Error.todo(range)
      }
      ctx.secondaryOffset += 8
    case LocalVarExpr(localVar, _, _) =>
      val stackOffset = ctx.secondaryOffset
      ctx.secondaryOffset += localVar.datatype.size.roundUp(8)
      localVar.datatype.generateCopyCode(ctx, Reg.RBP + stackOffset, Reg.RSP + ctx.lookup(localVar))
    case RefLocalVarExpr(localVar, _, _) =>
      ctx.add(
        Lea(Reg.RAX, Reg.RSP + ctx.lookup(localVar)),
        Store(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
      )
      ctx.secondaryOffset += 8
    case ValExpr(expr, _, _) =>
      expr.generateCode(ctx)
      ctx.secondaryOffset -= 8
      ctx.add(
        Load(Reg.RSI, Reg.RBP + ctx.secondaryOffset),
      )
      val datatype = expr.returnType.asInstanceOf[RefDatatype]
      datatype.generateCopyCode(ctx, Reg.RBP + ctx.secondaryOffset, Address(Reg.RSI))
      ctx.secondaryOffset += datatype.size.roundUp(8)
    case IntExpr(int, _, _) =>
      ctx.add(ConstInt(int).generateCode(Reg.RBP + ctx.secondaryOffset))
      ctx.secondaryOffset += 8
    case BoolExpr(bool, _, _) =>
      ctx.add(ConstBool(bool).generateCode(Reg.RBP + ctx.secondaryOffset))
      ctx.secondaryOffset += 8
    case TupleExpr(elements, _, _) =>
      val stackOffset = ctx.secondaryOffset
      val (tupleSize, _, tupleOffsets) = Datatype.alignSequence(elements.map(_.returnType))
      ctx.secondaryOffset += tupleSize.roundUp(8)
      val elementOffset = ctx.secondaryOffset
      for ((expr, offset) <- elements.zip(tupleOffsets)) {
        expr.generateCode(ctx)
        expr.returnType.generateCopyCode(ctx, Reg.RBP + (stackOffset + offset), Reg.RBP + elementOffset)
        ctx.secondaryOffset = elementOffset
      }
    case BlockExpr(exprs, lastExpr, vars, _, _) => ctx.scope({
      val varOffset = ctx.offset
      vars.foreach(ctx.add)
      ctx.offset = ctx.offset.roundDown(16)
      exprs.foreach(expr => {
        expr.generateCode(ctx)
      })
      lastExpr.generateCode(ctx)
      ctx.offset = varOffset
    })
    case UnitExpr(_, _) => ()
    case LetExpr(pattern, expr, _, _) =>
      val stackOffset = ctx.secondaryOffset
      expr.generateCode(ctx)

      def iteratePattern(pattern: Pattern[LocalVar], offset: Int): Unit = pattern match {
        case VarPattern(patternVar, _) => patternVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.lookup(patternVar), Reg.RBP + offset)
        case TuplePattern(elements, _) => elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3.map(_ + offset)).foreach(iteratePattern.tupled)
      }

      iteratePattern(pattern, stackOffset)
    case AssignExpr(LocalVarExpr(localVar, _, _), expr, _, _) =>
      val stackOffset = ctx.secondaryOffset
      expr.generateCode(ctx)
      localVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.lookup(localVar), Reg.RBP + stackOffset)
    case AssignExpr(GlobalVarExpr(globalVar, _, range), expr, _, _) =>
      val stackOffset = ctx.secondaryOffset
      expr.generateCode(ctx)
      globalVar.label match {
        case Some(label) => globalVar.datatype.generateCopyCode(ctx, Address(label), Reg.RBP + stackOffset)
        case None => throw Error.todo(range)
      }
    case AssignExpr(ValExpr(ptrExpr, _, _), expr, _, _) =>
      val stackOffset = ctx.secondaryOffset
      expr.generateCode(ctx)
      ptrExpr.generateCode(ctx)
      ctx.secondaryOffset -= 8
      ctx.add(Load(Reg.RSI, Reg.RBP + ctx.secondaryOffset))
      expr.returnType.generateCopyCode(ctx, Address(Reg.RSI), Reg.RBP + stackOffset)
    case FunExpr(fun, _, _) =>
      ctx.add(
        Lea(Reg.RAX, Address(fun.label)),
        Store(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
      )
      ctx.secondaryOffset += 8
    case FunTypeExpr(_, _, _, range) => Error.todo("", range)
    case RefTypeExpr(_, _, range) => Error.todo("", range)
    case IfExpr(condition, ifBlock, elseBlock, _, _) =>
      val baseOffset = ctx.secondaryOffset
      condition.generateCode(ctx)
      ctx.secondaryOffset = baseOffset

      val elseLabel = AsmGen.label()
      val endLabel = AsmGen.label()

      ctx.add(
        Xor(Reg.RAX, Reg.RAX),
        Load(Reg.RAX, Reg.RBP + ctx.secondaryOffset, RegSize.Byte),
        Cmp(Reg.RAX, 0),
        DirCondJump(elseLabel, Flag.Zero)
      )

      ifBlock.generateCode(ctx)
      ctx.secondaryOffset = baseOffset

      ctx.add(
        DirJump(endLabel),
        Label(elseLabel)
      )

      elseBlock.generateCode(ctx)

      ctx.add(Label(endLabel))
    case MutExpr(_, _, _, range) => Error.todo("", range)
  }

  def generateIr(nextCtrl: opt.Controlflow, localVars: Map[LocalVar, Int], counter: Counter): (opt.Dataflow, opt.Controlflow) = this match {
    case CallExpr(function, args, _, _) =>
      lazy val (fnExprData: opt.Dataflow, fnExprCtrl: opt.Controlflow) = function.generateIr(argsCtrl, localVars, counter)
      lazy val (argsCtrl: opt.Controlflow, argsData: List[opt.Dataflow]) = args.foldRight((callCtrl, List[opt.Dataflow]()))((arg, tuple) => {
        val (ctrl, argsData) = tuple
        lazy val (argData: opt.Dataflow, argCtrl: opt.Controlflow) = arg.generateIr(ctrl, localVars, counter)
        (argCtrl, argData :: argsData)
      })
      lazy val callOp: opt.Op = opt.CallInd(fnExprData, argsData, nextCtrl)

      lazy val callData: opt.Dataflow = opt.Dataflow(() => Some(callOp))
      lazy val callCtrl: opt.Controlflow = opt.Controlflow(() => callOp)
      (callData, fnExprCtrl)
    case GlobalVarExpr(globalVar, _, _) => ???
    case RefGlobalVarExpr(globalVar, _, _) => ???
    case LocalVarExpr(localVar, _, _) => ???
    case RefLocalVarExpr(localVar, _, _) => ???
    case ValExpr(expr, _, _) => ???
    case IntExpr(int, _, _) =>
      lazy val intOp: opt.Op = opt.IntLit(int, nextCtrl)
      lazy val intData: opt.Dataflow = opt.Dataflow(() => Some(intOp))
      lazy val intCtrl: opt.Controlflow = opt.Controlflow(() => intOp)
      (intData, intCtrl)
    case BoolExpr(bool, _, _) =>
      lazy val boolOp: opt.Op = opt.BoolLit(bool, nextCtrl)
      lazy val boolData: opt.Dataflow = opt.Dataflow(() => Some(boolOp))
      lazy val boolCtrl: opt.Controlflow = opt.Controlflow(() => boolOp)
      (boolData, boolCtrl)
    case TupleExpr(elements, _, _) => ???
    case BlockExpr(exprs, lastExpr, vars, _, _) =>
      val newLocalVars = if vars.nonEmpty then localVars.concat(vars.map(localVar => (localVar, counter.next))) else localVars
      lazy val exprsCtrl: opt.Controlflow = exprs.foldRight(lastExprCtrl)((expr, ctrl) => {
        expr.generateIr(ctrl, newLocalVars, counter)._2
      })

      lazy val (lastExprData: opt.Dataflow, lastExprCtrl: opt.Controlflow) = lastExpr.generateIr(nextCtrl, newLocalVars, counter)
      (lastExprData, exprsCtrl)
    case UnitExpr(_, _) =>
      lazy val unitOp: opt.Op = opt.UnitLit(nextCtrl)
      lazy val unitData: opt.Dataflow = opt.Dataflow(() => Some(unitOp))
      lazy val unitCtrl: opt.Controlflow = opt.Controlflow(() => unitOp)
      (unitData, unitCtrl)
    case LetExpr(pattern, expr, _, _) => 
      lazy val (exprData: opt.Dataflow, exprCtrl: opt.Controlflow) = expr.generateIr(patternCtrl, localVars, counter)
      lazy val patternCtrl: opt.Controlflow = Pattern.generateIr(pattern, exprData, nextCtrl, localVars)
      (exprData, exprCtrl)
    case AssignExpr(left, right, _, _) => ???
    case FunExpr(fun, _, _) => ???
    case FunTypeExpr(parameters, returnType, _, _) => ???
    case RefTypeExpr(expr, _, _) => ???
    case IfExpr(condition, ifBlock, elseBlock, _, _) => ???
    case MutExpr(expr, mutable, _, _) => ???
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
    case None => throw Error.todo(module.file)
  }))
  lazy val constVal: Option[ConstVal] = if datatype.mutable then None else letExpr.flatMap(_.constVal.map(patternNav.const))
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

class ExprCodeGenContext {
  private val instructions = mutable.Buffer[Instr]()
  var offset = 0
  var secondaryOffset = 0
  private var varOffsets = mutable.Map[LocalVar, Int]()
  def lookup(localVar: LocalVar): Int = varOffsets(localVar)

  def add(localVar: LocalVar): Int = {
    offset = offset.roundDown(localVar.datatype.align) - localVar.datatype.size
    varOffsets(localVar) = offset
    offset
  }

  def add(pattern: Pattern[LocalVar]): Unit = pattern match {
    case VarPattern(patternVar, _) => add(patternVar)
    case TuplePattern(elements, _) => elements.foreach(add)
  }

  def add(instr: Instr*): Unit = instr.foreach(add)

  def add(instr: Instr): Unit = if instr.redundant then () else instructions.append(instr)

  def add(instr: List[Instr]): Unit = instr.foreach(add)

  def scope[T](f: => T): T = {
    val copy = varOffsets.clone()
    val copyOffset = offset
    val value = f
    varOffsets = copy
    offset = copyOffset
    value
  }

  def code: List[Instr] = instructions.toList
}

def analyzeExpr(ctx: ExprParsingContext)(expr: syn.Expr): Expr = expr match {
  case syn.CallExpr(syn.IdenExpr(iden, funRange), posArgs, range) =>
    val overloadLayers = ctx.lookupOverload(iden)
    val analyzedPosArgs = posArgs.map(analyzeExpr(ctx))
    val appliedLayers = overloadLayers.map(overload(_, funRange, analyzedPosArgs))
    appliedLayers.find(_.nonEmpty).flatten match {
      case Some((globalVar: GlobalVar, args)) => CallExpr(GlobalVarExpr(globalVar, ctx.module, funRange), args, ctx.module, range)
      case Some((localVar: LocalVar, args)) => CallExpr(LocalVarExpr(localVar, ctx.module, funRange), args, ctx.module, range)
      case _ => throw Error.todo(funRange)
    }
  case syn.CallExpr(function, posArgs, range) =>
    val analyzedFunExpr = analyzeExpr(ctx)(function)
    analyzedFunExpr.returnType match {
      case FunDatatype(params, _, _) =>
        if (params.length != posArgs.length) throw Error.todo(range.file)
        val analyzedArgs = posArgs.map(analyzeExpr(ctx))
        if (!params.zip(analyzedArgs.map(_.returnType)).forall(t => t._1 == t._2)) throw Error.todo(range)
        CallExpr(analyzedFunExpr, analyzedArgs, ctx.module, range)
      case datatype => throw Error.semantic(s"'$datatype' is not callable", function.range)
    }
  case syn.IdenExpr(iden, range) => ctx.lookup(iden, range) match {
    case Some(globalVar: GlobalVar) => GlobalVarExpr(globalVar, ctx.module, range)
    case Some(localVar: LocalVar) => LocalVarExpr(localVar, ctx.module, range)
    case None => throw Error.todo(ctx.toString, range)
  }
  case syn.RefExpr(syn.IdenExpr(iden, idenRange), range) => ctx.lookup(iden, idenRange) match {
    case Some(globalVar: GlobalVar) => RefGlobalVarExpr(globalVar, ctx.module, range | idenRange)
    case Some(localVar: LocalVar) => RefLocalVarExpr(localVar, ctx.module, range | idenRange)
    case None => throw Error.todo(ctx.toString, range | idenRange)
  }
  case syn.RefExpr(expr, range) => RefTypeExpr(analyzeExpr(ctx)(expr), ctx.module, range)
  case syn.ValExpr(expr, range) =>
    val analyzedExpr = analyzeExpr(ctx)(expr)
    if (!analyzedExpr.returnType.isInstanceOf[RefDatatype]) throw Error.todo(ctx.module.file)
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
    lazy val (analyzedPattern: Pattern[LocalVar], vars: List[LocalVar]) = Pattern.map((pattern, patternNav) => new LocalVar(ctx.module, pattern.name, Some(letExpr), patternNav, pattern.datatype, pattern.range), pattern)
    if (analyzedExpr.returnType !~=> analyzedPattern.datatype) throw Error.typeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
    lazy val letExpr = LetExpr(analyzedPattern, analyzedExpr, ctx.module, range)
    ctx.add(vars)
    letExpr
  case syn.AssignExpr(left, right, range) =>
    val analyzedLeft = analyzeExpr(ctx)(left)
    val analyzedRight = analyzeExpr(ctx)(right)
    if (analyzedRight.returnType !~=> analyzedLeft.returnType) throw Error.typeMismatch(analyzedRight.returnType, analyzedLeft.returnType, analyzedRight.range, analyzedLeft.range)
    if (!analyzedLeft.returnType.mutable) throw Error.todo(analyzedLeft.range)
    analyzedLeft match {
      case _: (LocalVarExpr | GlobalVarExpr | ValExpr) => ()
      case _ => throw Error.todo(analyzedLeft.range)
    }
    AssignExpr(analyzedLeft, analyzedRight, ctx.module, range)
  case syn.FunExpr(parameters, returnType, expr, range) =>
    val fun = new UserFun(ctx.module, parameters, returnType, expr, range)
    FunExpr(fun, ctx.module, range)
  case syn.FunTypeExpr(parameters, returnType, range) => FunTypeExpr(parameters.map(analyzeExpr(ctx)), analyzeExpr(ctx)(returnType), ctx.module, range)
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
    if (analyzedExpr.returnType !~=> TypeDatatype(false)) throw Error(Error.SEMANTIC, kwRange.file, List(ErrorComponent(kwRange, Some(s"Expected 'Type', found '${analyzedExpr.returnType}'"))), Some("'mut' modifier is only applicable on expressions of type 'Type'"))
    MutExpr(analyzedExpr, mutable, ctx.module, range)
}
