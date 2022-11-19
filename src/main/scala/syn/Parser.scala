package syn

import core.*
import lex.*

import scala.annotation.tailrec

case class ParserState(tokens: List[Token], file: File) {
  def expectSymbol(symbol: String): ParserState = tokens match {
    case SymbolToken(sym, _) :: rest if sym == symbol => this withTokens rest
    case token :: _ => throw Error.unexpected(token, symbol)
    case _ => throw Error.unexpected(symbol, file)
  }

  def expectKeywordOptional(keyword: String): ParserState = tokens match {
    case KeywordToken(key, _) :: rest if key == keyword => this withTokens rest
    case _ => this
  }

  def withTokens(newTokens: List[Token]): ParserState = syn.ParserState(newTokens, file)
  def withTokensTail: ParserState = syn.ParserState(tokens.tail, file)
}

abstract class Expr {
  def range: FilePosRange

  def format(indentation: Int): String = this match {
    case CallExpr(fun, posArgs, _) => s"${fun.format(indentation)}(${posArgs.map(_.format(indentation)).mkString(", ")})"
    case IdenExpr(iden, _) => iden
    case RefExpr(expr, _) => s"ref ${expr.format(indentation)}"
    case ValExpr(expr, _) => s"val ${expr.format(indentation)}"
    case IntExpr(int, _) => int.toString
    case BoolExpr(bool, _) => bool.toString
    case TupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case BlockExpr(exprs, lastExpr, _) => s"{\n${exprs.map(expr => s"${" " * (indentation + 1)}${expr.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
    case UnitExpr(_) => "()"
    case DotExpr(expr, iden, _) => s"${expr.format(indentation)}.$iden"
    case LetExpr(pattern, expr, _) => s"let ${pattern.format(indentation)} = ${expr.format(indentation)}"
    case AssignExpr(left, right, _) => s"${left.format(indentation)} = ${right.format(indentation)}"
    case FunExpr(parameters, returnType, expr, _) => s"fn(${parameters.map(_.format(indentation)).mkString(", ")})${returnType.map(returnType => s": ${returnType.format(indentation)}").getOrElse("")} => ${expr.format(indentation)}"
    case FunTypeExpr(parameters, returnType, _) => s"(${parameters.map(_.format(indentation))}) => ${returnType.format(indentation)}"
    case IfExpr(condition, ifBlock, elseBlock, _) => s"if ${condition.format(indentation)} then ${ifBlock.format(indentation)} else ${elseBlock.format(indentation)}"
    case MutExpr(expr, mutable, _, _) => s"${if mutable then "mut" else "const"} ${expr.format(indentation)}"
  }
}

case class CallExpr(function: Expr, posArgs: List[Expr], range: FilePosRange) extends Expr

case class IdenExpr(iden: String, range: FilePosRange) extends Expr

case class RefExpr(expr: Expr, range: FilePosRange) extends Expr

case class ValExpr(expr: Expr, range: FilePosRange) extends Expr

case class IntExpr(int: Long, range: FilePosRange) extends Expr

case class BoolExpr(bool: Boolean, range: FilePosRange) extends Expr

case class TupleExpr(elements: List[Expr], range: FilePosRange) extends Expr

case class BlockExpr(exprs: List[Expr], lastExpr: Expr, range: FilePosRange) extends Expr

case class UnitExpr(range: FilePosRange) extends Expr

case class DotExpr(expr: Expr, iden: String, range: FilePosRange) extends Expr

case class LetExpr(pattern: Pattern, expr: Expr, range: FilePosRange) extends Expr

case class AssignExpr(left: Expr, right: Expr, range: FilePosRange) extends Expr

case class FunExpr(parameters: List[Pattern], returnType: Option[Expr], expr: Expr, range: FilePosRange) extends Expr

case class FunTypeExpr(parameters: List[Expr], returnType: Expr, range: FilePosRange) extends Expr

case class IfExpr(condition: Expr, ifBlock: Expr, elseBlock: Expr, range: FilePosRange) extends Expr

case class WhileExpr(condition: Expr, ifBlock: Expr, range: FilePosRange) extends Expr

case class MutExpr(expr: Expr, mutable: Boolean, range: FilePosRange, kwRange: FilePosRange) extends Expr

abstract class Pattern {
  def range: FilePosRange

  def format(indentation: Int): String = this match {
    case VarPattern(name, datatype, _) => s"$name${datatype.map(datatype => s": ${datatype.format(indentation)}").getOrElse("")}"
    case TuplePattern(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
  }
}

case class VarPattern(name: String, datatype: Option[Expr], range: FilePosRange) extends Pattern

case class TuplePattern(elements: List[Pattern], range: FilePosRange) extends Pattern

case class GlobalStmt(pattern: Pattern, expr: Expr, range: FilePosRange) {
  override def toString: String = s"${pattern.format(0)} = ${expr.format(0)}"
}

private def parseSep[T](state: ParserState, element: ParserState => (T, ParserState), sep: String, end: String, desc: String): (List[T], ParserState, FilePosRange) = {
  def rec(state: ParserState): (List[T], ParserState, FilePosRange) = if (state.tokens.isEmpty) {
    throw Error.unexpected(s"'$end' or '$sep'", state.file)
  } else if (state.tokens.head.isSymbol(sep)) {
    val (expr, nextIterTokens) = element(state.withTokensTail)
    val (exprs, rest, endRange) = rec(nextIterTokens)
    (expr :: exprs, rest, endRange)
  } else if (state.tokens.head.isSymbol(end)) {
    val endRange = state.tokens.head.range
    (List(), state.withTokensTail, endRange)
  } else {
    throw Error.unexpected(state.tokens.head, s"'$end' or '$sep'")
  }

  if (state.tokens.isEmpty) {
    throw Error.unexpected(s"'$end' or $desc", state.file)
  } else if (state.tokens.head.isSymbol(end)) {
    val endRange = state.tokens.head.range
    (List(), state.withTokensTail, endRange)
  } else {
    val (expr, newState) = element(state)
    val (exprs, rest, endRange) = rec(newState)
    (expr :: exprs, rest, endRange)
  }
}

private def parseStmt(state: ParserState): (Expr, ParserState) = state.tokens match {
  case SymbolToken("}", range) :: _ => (UnitExpr(range), state)
  case _ => parseExpr(0)(state)
}

private def parseExpr(precedence: Int)(state: ParserState): (Expr, ParserState) = state.tokens match {
  case IdenToken(iden, range) :: rest => parseExprAfter(precedence)(IdenExpr(iden, range), state withTokens rest)
  case IntToken(int, range) :: rest => parseExprAfter(precedence)(IntExpr(int, range), state withTokens rest)
  case KeywordToken("true", range) :: rest => parseExprAfter(precedence)(BoolExpr(true, range), state withTokens rest)
  case KeywordToken("false", range) :: rest => parseExprAfter(precedence)(BoolExpr(false, range), state withTokens rest)
  case KeywordToken("const", range) :: rest =>
    val (subExpr: Expr, newState: ParserState) = parseExpr(lex.MAX_PRECEDENCE)(state withTokens rest)
    parseExprAfter(precedence)(MutExpr(subExpr, false, range to subExpr.range, range), newState)
  case KeywordToken("mut", range) :: rest =>
    val (subExpr: Expr, newState: ParserState) = parseExpr(lex.MAX_PRECEDENCE)(state withTokens rest)
    parseExprAfter(precedence)(MutExpr(subExpr, true, range to subExpr.range, range), newState)
  case KeywordToken("ref", range) :: rest =>
    val (subExpr: Expr, newState: ParserState) = parseExpr(lex.MAX_PRECEDENCE)(state withTokens rest)
    parseExprAfter(precedence)(RefExpr(subExpr, range to subExpr.range), newState)
  case KeywordToken("val", range) :: rest =>
    val (subExpr: Expr, newState: ParserState) = parseExpr(lex.MAX_PRECEDENCE)(state withTokens rest)
    parseExprAfter(precedence)(ValExpr(subExpr, range to subExpr.range), newState)
  case SymbolToken("(", startRange) :: exprTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens exprTokens, parseExpr(0), ",", ")", "expression")
    newState.tokens match {
      case SymbolToken("=>", _) :: returnTypeTokens if lex.FUN_TYPE_LIT_PRECEDENCE >= precedence =>
        val (returnTypeExpr, lastState) = parseExpr(0)(newState withTokens returnTypeTokens)
        (FunTypeExpr(exprs, returnTypeExpr, startRange to returnTypeExpr.range), lastState)
      case _ => parseExprAfter(precedence)(exprs match {
        case List() => UnitExpr(startRange to endRange)
        case List(expr) => expr
        case exprList => TupleExpr(exprList, startRange to endRange)
      }, newState)
    }
  case SymbolToken("{", startRange) :: exprTokens =>
    val (body, newState, endRange) = parseSep(state withTokens exprTokens, parseStmt, ";", "}", "expression")
    val lastExpr = body.lastOption.getOrElse(UnitExpr(endRange))
    val exprs = body.dropRight(1)
    parseExprAfter(precedence)(BlockExpr(exprs, lastExpr, startRange to endRange), newState)
  case KeywordToken("let", startRange) :: patternTokens =>
    val (pattern, newState) = parsePattern(state withTokens patternTokens)
    val (expr, lastState) = parseExpr(0)(newState.expectSymbol("="))
    (LetExpr(pattern, expr, startRange to expr.range), lastState)
  case KeywordToken("fn", startRange) :: SymbolToken("(", _) :: parameterTokens =>
    val (parameters, newState, _) = parseSep(state withTokens parameterTokens, parsePattern, ",", ")", "pattern")
    newState.tokens match {
      case SymbolToken(":", _) :: retTypeTokens =>
        val (returnType, exprState) = parseExpr(lex.FUN_TYPE_LIT_PRECEDENCE + 1)(newState withTokens retTypeTokens)
        val (expr, lastState) = parseExpr(0)(exprState.expectSymbol("=>"))
        (FunExpr(parameters, Some(returnType), expr, startRange to expr.range), lastState)
      case _ =>
        val (expr, lastState) = parseExpr(0)(newState.expectSymbol("=>"))
        (FunExpr(parameters, None, expr, startRange to expr.range), lastState)
    }
  case KeywordToken("if", startRange) :: conditionTokens =>
    val (conditionExpr, bodyState) = parseExpr(0)(state withTokens conditionTokens)
    val (bodyExpr, elseState) = parseExpr(0)(bodyState.expectKeywordOptional("then"))
    elseState.tokens match {
      case KeywordToken("else", _) :: elseTokens =>
        val (elseBodyExpr, lastState) = parseExpr(0)(elseState withTokens elseTokens)
        parseExprAfter(0)(IfExpr(conditionExpr, bodyExpr, elseBodyExpr, startRange to elseBodyExpr.range), lastState)
      case _ => parseExprAfter(0)(IfExpr(conditionExpr, bodyExpr, UnitExpr(FilePosRange(bodyExpr.range.end, bodyExpr.range.end + 1, bodyExpr.range.file)), startRange to bodyExpr.range), elseState)
    }
  case token :: _ => throw Error.unexpected(token, "expression")
  case _ => throw Error.unexpected("expression", state.file)
}

@tailrec
private def parseExprAfter(precedence: Int)(expr: Expr, state: ParserState): (Expr, ParserState) = state.tokens match {
  case SymbolToken(".", _) :: IdenToken(op, opRange) :: SymbolToken("(", _) :: argumentTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens argumentTokens, parseExpr(0), ",", ")", "expression")
    parseExprAfter(precedence)(CallExpr(IdenExpr(op, opRange), expr :: exprs, expr.range to endRange), newState)
  case SymbolToken(".", _) :: IdenToken(iden, idenRange) :: rest => parseExprAfter(precedence)(DotExpr(expr, iden, expr.range to idenRange), state withTokens rest)
  case (idenToken: IdenToken) :: rightExprTokens if idenToken.precedence > precedence =>
    val (rightExpr: Expr, rest: ParserState) = parseExpr(idenToken.precedence)(state withTokens rightExprTokens)
    parseExprAfter(precedence)(CallExpr(IdenExpr(idenToken.iden, idenToken.range), List(expr, rightExpr), expr.range to rightExpr.range), rest)
  case SymbolToken("(", _) :: argumentTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens argumentTokens, parseExpr(0), ",", ")", "expression")
    parseExprAfter(precedence)(CallExpr(expr, exprs, expr.range to endRange), newState)
  case SymbolToken("=>", _) :: returnTypeTokens if lex.FUN_TYPE_LIT_PRECEDENCE >= precedence =>
    val (returnTypeExpr, lastState) = parseExpr(lex.FUN_TYPE_LIT_PRECEDENCE)(state withTokens returnTypeTokens)
    (FunTypeExpr(List(expr), returnTypeExpr, expr.range to returnTypeExpr.range), lastState)
  case SymbolToken("=", _) :: exprTokens if lex.ASSIGNMENT_PRECEDENCE >= precedence =>
    val (subExpr, lastState) = parseExpr(lex.ASSIGNMENT_PRECEDENCE)(state withTokens exprTokens)
    (AssignExpr(expr, subExpr, expr.range to subExpr.range), lastState)
  case _ => (expr, state)
}

private def parsePattern(state: ParserState): (Pattern, ParserState) = state.tokens match {
  case IdenToken(name, nameRange) :: SymbolToken(":", _) :: rest =>
    val (expr, newState) = parseExpr(lex.ASSIGNMENT_PRECEDENCE + 1)(state withTokens rest)
    (VarPattern(name, Some(expr), nameRange to expr.range), newState)
  case IdenToken(name, nameRange) :: rest =>
    (VarPattern(name, None, nameRange), state withTokens rest)
  case SymbolToken("(", startRange) :: rest =>
    val (patterns, newState, endRange) = parseSep(state withTokens rest, parsePattern, ",", ")", "pattern")
    patterns match {
      case List() => throw Error.todo(startRange to endRange)
      case List(pattern) => (pattern, newState)
      case patterns => (TuplePattern(patterns, startRange to endRange), newState)
    }
  case token :: _ => throw Error.unexpected(token, "pattern")
  case _ => throw Error.unexpected("pattern", state.file)
}

private def parseGlobalStmt(state: ParserState): (GlobalStmt, ParserState) = state.tokens match {
  case patternTokens =>
    val (pattern, newState) = parsePattern(state withTokens patternTokens)
    val (expr, lastState) = parseExpr(0)(newState.expectSymbol("="))
    (GlobalStmt(pattern, expr, pattern.range to expr.range), lastState)
}

@tailrec
private def parseGlobalStmts(stmts: List[GlobalStmt], state: ParserState): (List[GlobalStmt], ParserState) = state.tokens match {
  case tokens if tokens.nonEmpty =>
    val (stmt, newState) = parseGlobalStmt(state)
    parseGlobalStmts(stmt :: stmts, newState.expectSymbol(";"))
  case _ => (stmts, state)
}

def parseFile(tokens: List[Token], file: File): List[GlobalStmt] = parseGlobalStmts(List(), ParserState(tokens, file))._1.reverse
