package sem

import core.*
import gen.*

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable

abstract class Expr() {
  def range: FilePosRange

  lazy val returnType: Datatype = this match {
    case CallExpr(function, _, _) => function.returnType.asInstanceOf[FunDatatype].returnType
    case GlobalVarRefExpr(globalVar, _) => globalVar.datatype
    case LocalVarRefExpr(localVar, _) => localVar.datatype
    case IntExpr(_, _) => IntDatatype(false)
    case BoolExpr(_, _) => BoolDatatype(false)
    case TupleExpr(elements, _) => TupleDatatype(elements.map(_.returnType), false)
    case BlockExpr(_, lastExpr, _, _) => lastExpr.returnType
    case UnitExpr(_) => UnitDatatype(false)
    case LetExpr(_, expr, _) => expr.returnType
    case FunExpr(fun, _) => fun.signature
    case FunTypeExpr(_, _, _) => TypeDatatype(false)
    case IfExpr(_, ifBlock, _, _) => ifBlock.returnType
    case MutExpr(_, _, _) => TypeDatatype(false)
  }

  lazy val constVal: Option[ConstVal] = this match {
    case CallExpr(function, args, _) => for {
      fun <- function.constVal
      argValues <- args.map(_.constVal).extract
      returnValue <- fun.asInstanceOf[ConstFunction].function.constEval(argValues)
    } yield returnValue
    case GlobalVarRefExpr(globalVar, _) => globalVar.constVal
    case LocalVarRefExpr(localVar, _) => localVar.constVal
    case IntExpr(int, _) => Some(ConstInt(int))
    case BoolExpr(bool, _) => Some(ConstBool(bool))
    case TupleExpr(elements, _) => elements.map(_.constVal).extract.map(vals => ConstTuple(vals))
    case UnitExpr(_) => Some(ConstUnit)
    case LetExpr(_, expr, _) => expr.constVal
    case FunExpr(fun, _) => Some(ConstFunction(fun))
    case FunTypeExpr(parameters, returnType, _) => Some(ConstType(FunDatatype(parameters.map(_.constDatatype), returnType.constDatatype, false)))
    case IfExpr(condition, ifBlock, elseBlock, _) => condition.constVal.flatMap(evaluatedCondition => if evaluatedCondition.toBool then ifBlock.constVal else elseBlock.constVal)
    case MutExpr(expr, mutable, _) => for {
      constType <- expr.constVal
      datatype <- constType.toType
    } yield ConstType(datatype.withMut(mutable))
    case _ => constEval(ExprConstEvalContext())
  }

  lazy val constDatatype: Datatype = constVal.flatMap(_.toType) match {
    case Some(datatype) => datatype
    case None => throw Error.datatypeExpected(range)
  }

  def constEval(ctx: ExprConstEvalContext): Option[ConstVal] = this match {
    case CallExpr(function, args, _) => for {
      fun <- function.constEval(ctx)
      argValues <- args.map(_.constEval(ctx)).extract
      returnValue <- fun.asInstanceOf[ConstFunction].function.constEval(argValues)
    } yield returnValue
    case GlobalVarRefExpr(globalVar, _) => globalVar.constVal
    case LocalVarRefExpr(localVar, _) => ctx.lookup(localVar)
    case IntExpr(int, _) => Some(ConstInt(int))
    case BoolExpr(bool, _) => Some(ConstBool(bool))
    case TupleExpr(elements, _) => elements.map(element => element.constEval(ctx)).extract.map(vals => ConstTuple(vals))
    case BlockExpr(exprs, lastExpr, _, _) => ctx.scope(if exprs.map(_.constEval(ctx)).forall(_.nonEmpty) then lastExpr.constEval(ctx) else None)
    case UnitExpr(_) => Some(ConstUnit)
    case LetExpr(pattern, expr, _) => expr.constEval(ctx).map(value => ctx.add(pattern, value))
    case FunExpr(fun, _) => Some(ConstFunction(fun))
    case FunTypeExpr(parameters, returnType, _) => Some(ConstType(FunDatatype(parameters.map(_.constDatatype), returnType.constDatatype, false)))
    case IfExpr(condition, ifBlock, elseBlock, _) => condition.constEval(ctx).flatMap(evaluatedCondition => if evaluatedCondition.toBool then ifBlock.constEval(ctx) else elseBlock.constEval(ctx))
    case MutExpr(expr, mutable, _) => for {
      constType <- expr.constEval(ctx)
      datatype <- constType.toType
    } yield ConstType(datatype.withMut(mutable))
  }

  def generateCode(ctx: ExprCodeGenContext, topLevel: Boolean = false): Unit = constVal match {
    case Some(constVal) => if (!topLevel) {
      ctx.add(constVal.generateCode(Reg.RBP + ctx.secondaryOffset))
    }
    case None => this match {
      case CallExpr(functionExpr, args, _) => functionExpr.constVal match {
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
      case GlobalVarRefExpr(globalVar, range) =>
        val stackOffset = ctx.secondaryOffset
        ctx.secondaryOffset += globalVar.datatype.size.roundUp(8)
        globalVar.label match {
          case Some(label) => globalVar.datatype.generateCopyCode(ctx, Reg.RBP + stackOffset, Address(label))
          case None => throw Error.todo(range)
        }
      case LocalVarRefExpr(localVar, _) =>
        val stackOffset = ctx.secondaryOffset
        ctx.secondaryOffset += localVar.datatype.size.roundUp(8)
        localVar.datatype.generateCopyCode(ctx, Reg.RBP + stackOffset, Reg.RSP + ctx.lookup(localVar))
      case IntExpr(int, _) =>
        ctx.add(ConstInt(int).generateCode(Reg.RBP + ctx.secondaryOffset))
        ctx.secondaryOffset += 8
      case BoolExpr(bool, _) =>
        ctx.add(ConstBool(bool).generateCode(Reg.RBP + ctx.secondaryOffset))
        ctx.secondaryOffset += 8
      case TupleExpr(elements, _) =>
        val stackOffset = ctx.secondaryOffset
        val (tupleSize, _, tupleOffsets) = Datatype.alignSequence(elements.map(_.returnType))
        ctx.secondaryOffset += tupleSize.roundUp(8)
        val elementOffset = ctx.secondaryOffset
        for ((expr, offset) <- elements.zip(tupleOffsets)) {
          expr.generateCode(ctx)
          expr.returnType.generateCopyCode(ctx, Reg.RBP + (stackOffset + offset), Reg.RBP + elementOffset)
          ctx.secondaryOffset = elementOffset
        }
      case BlockExpr(exprs, lastExpr, vars, _) => ctx.scope({
        val varOffset = ctx.offset
        vars.foreach(ctx.add)
        ctx.offset = ctx.offset.roundDown(16)
        exprs.foreach(expr => {
          expr.generateCode(ctx, true)
        })
        lastExpr.generateCode(ctx)
        ctx.offset = varOffset
      })
      case UnitExpr(_) => ()
      case LetExpr(pattern, expr, _) =>
        val stackOffset = ctx.secondaryOffset
        expr.generateCode(ctx)

        def iteratePattern(pattern: Pattern[LocalVar], offset: Int): Unit = pattern match {
          case VarPattern(patternVar, _) => patternVar.datatype.generateCopyCode(ctx, Reg.RSP + ctx.lookup(patternVar), Reg.RBP + offset)
          case TuplePattern(elements, _) => elements.zip(Datatype.alignSequence(elements.map(_.datatype))._3.map(_ + offset)).foreach(iteratePattern.tupled)
        }

        iteratePattern(pattern, stackOffset)
      case FunExpr(fun, _) =>
        ctx.add(
          Lea(Reg.RAX, Address(fun.label)),
          Store(Reg.RBP + ctx.secondaryOffset, Reg.RAX)
        )
        ctx.secondaryOffset += 8
      case FunTypeExpr(_, _, range) => Error.todo("", range)
      case IfExpr(condition, ifBlock, elseBlock, _) =>
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
      case MutExpr(_, _, range) => Error.todo("", range)
    }
  }

  def format(indentation: Int): String = this match {
    case CallExpr(function, args, _) => s"${function.format(indentation)}(${args.map(_.format(indentation)).mkString(", ")})"
    case GlobalVarRefExpr(globalVar, _) => s"${globalVar.name}"
    case LocalVarRefExpr(localVar, _) => s"${localVar.name}"
    case IntExpr(int, _) => s"$int"
    case BoolExpr(bool, _) => s"$bool"
    case TupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case BlockExpr(exprs, lastExpr, _, _) => s"{\n${exprs.map(e => s"${" " * (indentation + 1)}${e.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
    case UnitExpr(_) => s"()"
    case LetExpr(pattern, expr, _) => s"let ${pattern.format(indentation)} = ${expr.format(indentation)}"
    case FunExpr(fun, _) => s"${fun.format(indentation)}"
    case FunTypeExpr(parameters, returnType, _) => s"(${parameters.map(_.format(indentation)).mkString(", ")}) => ${returnType.format(indentation)}"
    case IfExpr(condition, ifBlock, elseBlock, _) => s"if ${condition.format(indentation)} then ${ifBlock.format(indentation)} else ${elseBlock.format(indentation)}"
    case MutExpr(expr, mutable, _) => s"${if mutable then "mut" else "const"} ${expr.format(indentation)}"
  }
}

case class CallExpr(function: Expr, args: List[Expr], range: FilePosRange) extends Expr

case class GlobalVarRefExpr(globalVar: GlobalVar, range: FilePosRange) extends Expr

case class LocalVarRefExpr(localVar: LocalVar, range: FilePosRange) extends Expr

case class IntExpr(int: Long, range: FilePosRange) extends Expr

case class BoolExpr(bool: Boolean, range: FilePosRange) extends Expr

case class TupleExpr(elements: List[Expr], range: FilePosRange) extends Expr

case class BlockExpr(exprs: List[Expr], lastExpr: Expr, vars: List[LocalVar], range: FilePosRange) extends Expr

case class UnitExpr(range: FilePosRange) extends Expr

case class LetExpr(pattern: Pattern[LocalVar], expr: Expr, range: FilePosRange) extends Expr

case class FunExpr(fun: Fun, range: FilePosRange) extends Expr

case class FunTypeExpr(parameters: List[Expr], returnTypeExpr: Expr, range: FilePosRange) extends Expr

case class IfExpr(condition: Expr, ifBlock: Expr, elseBlock: Expr, range: FilePosRange) extends Expr

case class MutExpr(expr: Expr, mutable: Boolean, range: FilePosRange) extends Expr

class LocalVar(val module: Module, val name: String, letExpr: => Option[LetExpr], patternNav: PatternNav, typeExpr: Option[syn.Expr], val range: FilePosRange) extends Var {
  lazy val datatype: Datatype = typeExpr.map(typeExpr => analyzeExpr(ExprParsingContext(module, None))(typeExpr).constDatatype).getOrElse(patternNav.datatype(letExpr match {
    case Some(letExpr) => letExpr.expr.returnType
    case None => throw Error.todo(module.file)
  }))
  lazy val constVal: Option[ConstVal] = if datatype.mutable then None else letExpr.flatMap(_.constVal.map(patternNav.const))
}

abstract class OverloadLayer {
  def apply(funRange: FilePosRange, posArgs: List[Expr], keywordArgs: List[(String, Expr)]): Option[(Var, List[Expr])]
}

case class LocalOverloadLayer(localVar: LocalVar) extends OverloadLayer {
  override def apply(funRange: FilePosRange, posArgs: List[Expr], keywordArgs: List[(String, Expr)]): Option[(Var, List[Expr])] = (posArgs, keywordArgs, localVar.datatype) match {
    case (posArgs, List(), funDatatype: FunDatatype) if funDatatype.params.zip(posArgs.map(_.returnType)).forall(_ ~= _) => Some((localVar, posArgs))
    case _ => None
  }
}

case class GlobalOverloadLayer(globalVars: List[GlobalVar]) extends OverloadLayer {
  override def apply(funRange: FilePosRange, posArgs: List[Expr], keywordArgs: List[(String, Expr)]): Option[(Var, List[Expr])] = {
    def fit(globalVar: GlobalVar): Option[(Var, List[Expr])] = (posArgs, keywordArgs, globalVar.constVal) match {
      case (posArgs, keywordArgs, Some(ConstFunction(function))) =>
        val (noArgs, keywordAsPosArgs) = keywordArgs.partitionMap { case (str, expr) =>
          function.argNameToIndex.get(str) match {
            case Some(index) => Right((expr, index))
            case None => Left(expr)
          }
        }
        if (noArgs.nonEmpty) return None
        val (multipleArgs, args) = (posArgs.zipWithIndex ::: keywordAsPosArgs).groupMap(_._2)(_._1).partitionMap {
          case (index, List(arg)) => Right((index, arg))
          case (index, args) => Left((index, args))
        }
        if (multipleArgs.nonEmpty) throw Error.todo(funRange.file)
        val argsMap = args.map(_._1).toSet
        if (Range(0, function.argTypes.length).forall(argsMap.contains)) {
          Some(args.toList.sortWith((t1, t2) => t1._1 < t2._1).map(_._2)).filter(params => function.argTypes.zip(params.map(_.returnType)).forall(_ ~= _)).map((globalVar, _))
        } else
          None
      case (posArgs, List(), _) => globalVar.datatype match {
        case funDatatype: FunDatatype if funDatatype.params == posArgs.map(_.returnType) => Some((globalVar, posArgs))
        case _ => None
      }
      case _ => None
    }

    globalVars.map(fit).filter(_.nonEmpty).map(_.get) match {
      case List() => None
      case List(single) => Some(single)
      case varList => throw Error(Error.SEMANTIC, funRange.file, ErrorComponent(funRange, Some("Multiple matches for overloaded reference")) :: varList.map { case (globalVar, _) => ErrorComponent(globalVar.range, Some("One match")) }, Some("Ambiguous reference"))
    }
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

  def lookupOverload(name: String): List[OverloadLayer] = {
    def rec(list: List[LocalVar]): List[OverloadLayer] = if (list.isEmpty) {
      fun.flatMap(_.params.get(name)).map(localVar => LocalOverloadLayer(localVar)).toList ::: module.vars.get(name).map(globalVars => GlobalOverloadLayer(globalVars)).toList
    } else if (list.head.name == name) {
      LocalOverloadLayer(list.head) :: rec(list.tail)
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

class ExprConstEvalContext {
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
  case syn.CallExpr(syn.IdenExpr(iden, funRange), posArgs, keywordArgs, range) =>
    val overloadLayers = ctx.lookupOverload(iden)
    val analyzedPosArgs = posArgs.map(analyzeExpr(ctx))
    val analyzedKeywordArgs = keywordArgs.map { case (name, argExpr) => (name, analyzeExpr(ctx)(argExpr)) }
    val appliedLayers = overloadLayers.map(_.apply(funRange, analyzedPosArgs, analyzedKeywordArgs))
    appliedLayers.find(_.nonEmpty).flatten match {
      case Some((globalVar: GlobalVar, args)) => CallExpr(GlobalVarRefExpr(globalVar, funRange), args, range)
      case Some((localVar: LocalVar, args)) => CallExpr(LocalVarRefExpr(localVar, funRange), args, range)
      case _ => throw Error.todo(funRange)
    }
  case syn.CallExpr(function, posArgs, List(), range) =>
    val analyzedFunExpr = analyzeExpr(ctx)(function)
    analyzedFunExpr.returnType match {
      case FunDatatype(params, _, _) =>
        if (params.length != posArgs.length) throw Error.todo(range.file)
        val analyzedArgs = posArgs.map(analyzeExpr(ctx))
        if (!params.zip(analyzedArgs.map(_.returnType)).forall(t => t._1 == t._2)) throw Error.todo(range)
        CallExpr(analyzedFunExpr, analyzedArgs, range)
      case datatype => throw Error.semantic(s"'$datatype' is not callable", function.range)
    }
  case syn.CallExpr(function, _, keywordArgs, range) => throw Error(Error.SEMANTIC, range.file,
    ErrorComponent(function.range, Some("This is an expression that, when evaluated, returns a function\nIn order to use keyword arguments, a direct reference is required")) :: keywordArgs.map { case (_, argExpr) => ErrorComponent(argExpr.range, Some("Disallowed keyword argument")) },
    Some("Keyword arguments are not allowed for indirect function calls")
  )
  case syn.IdenExpr(iden, range) => ctx.lookup(iden, range) match {
    case Some(globalVar: GlobalVar) => GlobalVarRefExpr(globalVar, range)
    case Some(localVar: LocalVar) => LocalVarRefExpr(localVar, range)
    case None => throw Error.todo(ctx.toString, range)
  }
  case syn.IntExpr(int, range) => IntExpr(int, range)
  case syn.BoolExpr(bool, range) => BoolExpr(bool, range)
  case syn.TupleExpr(elements, range) => TupleExpr(elements.map(analyzeExpr(ctx)), range)
  case syn.BlockExpr(exprs, lastExpr, range) =>
    val ((analyzedExprs, analyzedLastExpr), vars) = ctx.scope((exprs.map(analyzeExpr(ctx)), analyzeExpr(ctx)(lastExpr)))
    BlockExpr(analyzedExprs, analyzedLastExpr, vars, range)
  case syn.UnitExpr(range) => UnitExpr(range)
  case syn.LetExpr(pattern, expr, range) =>
    lazy val analyzedExpr = analyzeExpr(ctx)(expr)
    lazy val (analyzedPattern: Pattern[LocalVar], vars: List[LocalVar]) = Pattern.map((pattern, patternNav) => new LocalVar(ctx.module, pattern.name, Some(letExpr), patternNav, pattern.datatype, pattern.range), pattern)
    if (analyzedPattern.datatype !~= analyzedExpr.returnType) throw Error.typeMismatch(analyzedExpr.returnType, analyzedPattern.datatype, analyzedExpr.range, analyzedPattern.range)
    lazy val letExpr = LetExpr(analyzedPattern, analyzedExpr, range)
    ctx.add(vars)
    letExpr
  case syn.FunExpr(parameters, returnType, expr, range) => {
    val fun = new UserFun(ctx.module, parameters, returnType, expr, range)
    FunExpr(fun, range)
  }
  case syn.FunTypeExpr(parameters, returnType, range) => FunTypeExpr(parameters.map(analyzeExpr(ctx)), analyzeExpr(ctx)(returnType), range)
  case syn.IfExpr(condition, ifBlock, elseBlock, range) =>
    val analyzedCondition = analyzeExpr(ctx)(condition)
    if (analyzedCondition.returnType !~= BoolDatatype(false)) throw Error(Error.SEMANTIC, range.file, List(ErrorComponent(analyzedCondition.range, Some(s"Expected 'Bool', found '${analyzedCondition.returnType}'"))))
    val analyzedIfBlock = analyzeExpr(ctx)(ifBlock)
    val analyzedElseBlock = analyzeExpr(ctx)(elseBlock)
    if (analyzedIfBlock.returnType !~= analyzedElseBlock.returnType) throw Error(Error.SEMANTIC, range.file, List(
      ErrorComponent(analyzedIfBlock.range, Some(s"The if branch returns value of type '${analyzedIfBlock.returnType}'")),
      ErrorComponent(analyzedElseBlock.range, Some(s"The else branch returns value of type '${analyzedElseBlock.returnType}'"))
    ), Some("Both branches of an if statement must return values of the same type"))
    IfExpr(analyzedCondition, analyzedIfBlock, analyzedElseBlock, range)
  case syn.MutExpr(expr, mutable, range, kwRange) =>
    val analyzedExpr = analyzeExpr(ctx)(expr)
    if (analyzedExpr.returnType !~= TypeDatatype(false)) throw Error(Error.SEMANTIC, kwRange.file, List(ErrorComponent(kwRange, Some(s"Expected 'Type', found '${analyzedExpr.returnType}'"))), Some("'mut' modifier is only applicable on expressions of type 'Type'"))
    MutExpr(analyzedExpr, mutable, range)
}
