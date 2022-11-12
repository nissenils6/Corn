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
    case CallExpr(fun, posArgs, keywordArgs, _) => s"${fun.format(indentation)}(${posArgs.map(_.format(indentation)).mkString(", ")}${keywordArgs.map(t => s", ${t._1} = ${t._2.format(indentation)}").mkString})"
    case RefExpr(iden, _) => iden
    case IntExpr(int, _) => int.toString
    case BoolExpr(bool, _) => bool.toString
    case TupleExpr(elements, _) => s"(${elements.map(_.format(indentation)).mkString(", ")})"
    case BlockExpr(exprs, lastExpr, _) => s"{\n${exprs.map(expr => s"${" " * (indentation + 1)}${expr.format(indentation + 1)};\n").mkString}${" " * (indentation + 1)}${lastExpr.format(indentation + 1)}\n${" " * indentation}}"
    case UnitExpr(_) => "()"
    case DotExpr(expr, iden, _) => s"${expr.format(indentation)}.$iden"
    case LetExpr(pattern, expr, _) => s"let ${pattern.format(indentation)} = ${expr.format(indentation)}"
    case FunExpr(parameters, returnType, expr, _) => s"fn(${parameters.map(_.format(indentation)).mkString(", ")})${returnType.map(returnType => s": ${returnType.format(indentation)}").getOrElse("")} => ${expr.format(indentation)}"
    case FunTypeExpr(parameters, returnType, _) => s"(${parameters.map(_.format(indentation))}) => ${returnType.format(indentation)}"
    case IfExpr(condition, ifBlock, elseBlock, _) => s"if ${condition.format(indentation)} then ${ifBlock.format(indentation)} else ${elseBlock.format(indentation)}"
    case MutExpr(expr, mutable, _, _) => s"${if mutable then "mut" else "const"} ${expr.format(indentation)}"
  }
}

case class CallExpr(function: Expr, posArgs: List[Expr], keywordArgs: List[(String, Expr)], range: FilePosRange) extends Expr

case class RefExpr(iden: String, range: FilePosRange) extends Expr

case class IntExpr(int: Long, range: FilePosRange) extends Expr

case class BoolExpr(bool: Boolean, range: FilePosRange) extends Expr

case class TupleExpr(elements: List[Expr], range: FilePosRange) extends Expr

case class BlockExpr(exprs: List[Expr], lastExpr: Expr, range: FilePosRange) extends Expr

case class UnitExpr(range: FilePosRange) extends Expr

case class DotExpr(expr: Expr, iden: String, range: FilePosRange) extends Expr

case class LetExpr(pattern: Pattern, expr: Expr, range: FilePosRange) extends Expr

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

private def parseArgument(state: ParserState): ((Option[String], Expr), ParserState) = state.tokens match {
  case IdenToken(name, _) :: SymbolToken("=", _) :: exprTokens =>
    val (expr, rest) = parseExpr(0, true)(state withTokens exprTokens)
    ((Some(name), expr), rest)
  case exprTokens =>
    val (expr, rest) = parseExpr(0, true)(state withTokens exprTokens)
    ((None, expr), rest)
}

private def parseStmt(state: ParserState): (Expr, ParserState) = state.tokens match {
  case SymbolToken("}", range) :: _ => (UnitExpr(range), state)
  case _ => parseExpr(0, true)(state)
}

// todo: Wrap entire thing in parseExpr(precedence, allowFunTypeLiterals)(...)
private def parseExpr(precedence: Int, allowFunTypeLiterals: Boolean)(state: ParserState): (Expr, ParserState) = state.tokens match {
  case IdenToken(iden, range) :: rest => parseExpr(precedence, allowFunTypeLiterals)(RefExpr(iden, range), state withTokens rest)
  case IntToken(int, range) :: rest => parseExpr(precedence, allowFunTypeLiterals)(IntExpr(int, range), state withTokens rest)
  case KeywordToken("true", range) :: rest => parseExpr(precedence, allowFunTypeLiterals)(BoolExpr(true, range), state withTokens rest)
  case KeywordToken("false", range) :: rest => parseExpr(precedence, allowFunTypeLiterals)(BoolExpr(false, range), state withTokens rest)
  case KeywordToken("const", range) :: rest =>
    val (subExpr: Expr, newState: ParserState) = parseExpr(lex.MAX_PRECEDENCE, allowFunTypeLiterals)(state withTokens rest)
    parseExpr(precedence, allowFunTypeLiterals)(MutExpr(subExpr, false, range to subExpr.range, range), newState)
  case KeywordToken("mut", range) :: rest =>
    val (subExpr: Expr, newState: ParserState) = parseExpr(lex.MAX_PRECEDENCE, allowFunTypeLiterals)(state withTokens rest)
    parseExpr(precedence, allowFunTypeLiterals)(MutExpr(subExpr, true, range to subExpr.range, range), newState)
  case SymbolToken("(", startRange) :: exprTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens exprTokens, parseExpr(0, true), ",", ")", "expression")
    newState.tokens match {
      case SymbolToken("=>", _) :: returnTypeTokens if allowFunTypeLiterals =>
        val (returnTypeExpr, lastState) = parseExpr(0, true)(newState withTokens returnTypeTokens)
        (FunTypeExpr(exprs, returnTypeExpr, startRange to returnTypeExpr.range), lastState)
      case _ => parseExpr(precedence, allowFunTypeLiterals)(exprs match {
        case List() => UnitExpr(startRange to endRange)
        case List(expr) => expr
        case exprList => TupleExpr(exprList, startRange to endRange)
      }, newState)
    }
  case SymbolToken("{", startRange) :: exprTokens =>
    val (body, newState, endRange) = parseSep(state withTokens exprTokens, parseStmt, ";", "}", "expression")
    val lastExpr = body.lastOption.getOrElse(UnitExpr(endRange))
    val exprs = body.dropRight(1)
    parseExpr(precedence, allowFunTypeLiterals)(BlockExpr(exprs, lastExpr, startRange to endRange), newState)
  case KeywordToken("let", startRange) :: patternTokens =>
    val (pattern, newState) = parsePattern(state withTokens patternTokens)
    val (expr, lastState) = parseExpr(0, true)(newState.expectSymbol("="))
    (LetExpr(pattern, expr, startRange to expr.range), lastState)
  case KeywordToken("fn", startRange) :: SymbolToken("(", _) :: parameterTokens =>
    val (parameters, newState, _) = parseSep(state withTokens parameterTokens, parsePattern, ",", ")", "pattern")
    newState.tokens match {
      case SymbolToken(":", _) :: retTypeTokens =>
        val (returnType, exprState) = parseExpr(0, false)(newState withTokens retTypeTokens)
        val (expr, lastState) = parseExpr(0, true)(exprState.expectSymbol("=>"))
        (FunExpr(parameters, Some(returnType), expr, startRange to expr.range), lastState)
      case _ =>
        val (expr, lastState) = parseExpr(0, true)(newState.expectSymbol("=>"))
        (FunExpr(parameters, None, expr, startRange to expr.range), lastState)
    }
  case KeywordToken("if", startRange) :: conditionTokens =>
    val (conditionExpr, bodyState) = parseExpr(0, true)(state withTokens conditionTokens)
    val (bodyExpr, elseState) = parseExpr(0, true)(bodyState.expectKeywordOptional("then"))
    elseState.tokens match {
      case KeywordToken("else", _) :: elseTokens =>
        val (elseBodyExpr, lastState) = parseExpr(0, true)(elseState withTokens elseTokens)
        parseExpr(0, true)(IfExpr(conditionExpr, bodyExpr, elseBodyExpr, startRange to elseBodyExpr.range), lastState)
      case _ => parseExpr(0, true)(IfExpr(conditionExpr, bodyExpr, UnitExpr(FilePos(bodyExpr.range.end, bodyExpr.range.file).range), startRange to bodyExpr.range), elseState)
    }
  case token :: _ => throw Error.unexpected(token, "expression")
  case _ => throw Error.unexpected("expression", state.file)
}

@tailrec
private def parseExpr(precedence: Int, allowFunTypeLiterals: Boolean)(expr: Expr, state: ParserState): (Expr, ParserState) = state.tokens match {
  case SymbolToken(".", _) :: IdenToken(op, opRange) :: SymbolToken("(", _) :: argumentTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens argumentTokens, parseArgument, ",", ")", "expression")
    val posArgs = exprs.filter(_._1.isEmpty).map(_._2)
    val keywordArgs = exprs.filter(_._1.nonEmpty).map(t => (t._1.get, t._2))
    parseExpr(precedence, allowFunTypeLiterals)(CallExpr(RefExpr(op, opRange), expr :: posArgs, keywordArgs, expr.range to endRange), newState)
  case SymbolToken(".", _) :: IdenToken(iden, idenRange) :: rest => parseExpr(precedence, allowFunTypeLiterals)(DotExpr(expr, iden, expr.range to idenRange), state withTokens rest)
  case (idenToken: IdenToken) :: rightExprTokens if idenToken.precedence > precedence =>
    val (rightExpr, rest) = parseExpr(idenToken.precedence, true)(state withTokens rightExprTokens)
    parseExpr(precedence, allowFunTypeLiterals)(CallExpr(RefExpr(idenToken.iden, idenToken.range), List(expr, rightExpr), List(), expr.range to rightExpr.range), rest)
  case SymbolToken("(", _) :: argumentTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens argumentTokens, parseArgument, ",", ")", "expression")
    val posArgs = exprs.filter(_._1.isEmpty).map(_._2)
    val keywordArgs = exprs.filter(_._1.nonEmpty).map(t => (t._1.get, t._2))
    parseExpr(precedence, allowFunTypeLiterals)(CallExpr(expr, posArgs, keywordArgs, expr.range to endRange), newState)
  case SymbolToken("=>", _) :: returnTypeTokens if allowFunTypeLiterals =>
    val (returnTypeExpr, lastState) = parseExpr(0, true)(state withTokens returnTypeTokens)
    (FunTypeExpr(List(expr), returnTypeExpr, expr.range to returnTypeExpr.range), lastState)
  case _ => (expr, state)
}

private def parsePattern(state: ParserState): (Pattern, ParserState) = state.tokens match {
  case IdenToken(name, nameRange) :: SymbolToken(":", _) :: rest =>
    val (expr, newState) = parseExpr(0, true)(state withTokens rest)
    (VarPattern(name, Some(expr), nameRange to expr.range), newState)
  case IdenToken(name, nameRange) :: rest =>
    (VarPattern(name, None, nameRange), state withTokens rest)
  case SymbolToken("(", startRange) :: rest =>
    val (patterns, newState, endRange) = parseSep(state withTokens rest, parsePattern, ",", ")", "pattern")
    (TuplePattern(patterns, startRange to endRange), newState)
  case token :: _ => throw Error.unexpected(token, "pattern")
  case _ => throw Error.unexpected("pattern", state.file)
}

private def parseGlobalStmt(state: ParserState): (GlobalStmt, ParserState) = state.tokens match {
  case patternTokens =>
    val (pattern, newState) = parsePattern(state withTokens patternTokens)
    val (expr, lastState) = parseExpr(0, true)(newState.expectSymbol("="))
    (GlobalStmt(pattern, expr, pattern.range to expr.range), lastState)
}

@tailrec
def parseGlobalStmts(stmts: List[GlobalStmt], state: ParserState): (List[GlobalStmt], ParserState) = state.tokens match {
  case tokens if tokens.nonEmpty =>
    val (stmt, newState) = parseGlobalStmt(state)
    parseGlobalStmts(stmt :: stmts, newState.expectSymbol(";"))
  case _ => (stmts, state)
}
