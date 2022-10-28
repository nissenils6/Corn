import scala.annotation.tailrec

case class ParserState(tokens: List[Token], file: File) {
  def withTokens(newTokens: List[Token]): ParserState = ParserState(newTokens, file)

  def withTokensTail: ParserState = ParserState(tokens.tail, file)

  def expectKeyword(keyword: String): ParserState = tokens match {
    case KeywordToken(key, _) :: rest if key == keyword => this withTokens rest
    case token :: _ => throw Error.unexpected(token, keyword)
    case _ => throw Error.unexpected(keyword, file)
  }

  def expectSymbol(symbol: String): ParserState = tokens match {
    case SymbolToken(sym, _) :: rest if sym == symbol => this withTokens rest
    case token :: _ => throw Error.unexpected(token, symbol)
    case _ => throw Error.unexpected(symbol, file)
  }
}

abstract class Expr() {
  def range: FilePosRange

  def format(i: Int): String = this match {
    case CallExpr(fun, posArgs, keywordArgs, _) => s"$fun(${posArgs.mkString(", ")}${keywordArgs.map(t => s", ${t._1} = ${t._2}").mkString})"
    case RefExpr(iden, _) => iden
    case IntExpr(int, _) => int.toString
    case TupleExpr(elements, _) => s"(${elements.mkString(", ")})"
    case BlockExpr(exprs, lastExpr, _) =>
      s"{\n${exprs.map(expr => s"${" " * (i + 1)}${expr.format(i + 1)};\n").mkString}${" " * (i + 1)}${lastExpr.format(i + 1)}\n${" " * i}}"
    case UnitExpr(_) => "()"
    case DotExpr(expr, iden, _) => s"${expr.format(i)}.$iden"
    case LetExpr(pattern, expr, _) => s"let $pattern = ${expr.format(i)}"
  }

  override def toString: String = format(0)
}

case class CallExpr(function: Expr, posArgs: List[Expr], keywordArgs: List[(String, Expr)], range: FilePosRange) extends Expr

case class RefExpr(iden: String, range: FilePosRange) extends Expr

case class IntExpr(int: Int, range: FilePosRange) extends Expr

case class TupleExpr(elements: List[Expr], range: FilePosRange) extends Expr

case class BlockExpr(exprs: List[Expr], lastExpr: Expr, range: FilePosRange) extends Expr

case class UnitExpr(range: FilePosRange) extends Expr

case class DotExpr(expr: Expr, iden: String, range: FilePosRange) extends Expr

case class LetExpr(pattern: Pattern, expr: Expr, range: FilePosRange) extends Expr

abstract class Pattern {
  def range: FilePosRange

  override def toString: String = this match {
    case VarPattern(name, dataType, _) => s"$name: $dataType"
    case TuplePattern(elements, _) => s"(${elements.mkString(", ")})"
  }
}

case class VarPattern(name: String, datatype: Expr, range: FilePosRange) extends Pattern

case class TuplePattern(elements: List[Pattern], range: FilePosRange) extends Pattern

abstract class GlobalStmt {
  def range: FilePosRange

  override def toString: String = this match {
    case LetGlobalStmt(pattern, expr, _) => s"let $pattern = $expr"
    case FunGlobalStmt(name, parameters, returnType, expr, _) =>
      s"fun $name(${parameters.mkString(", ")}): $returnType => $expr"
  }
}

case class LetGlobalStmt(pattern: Pattern, expr: Expr, range: FilePosRange) extends GlobalStmt

case class FunGlobalStmt(name: String, parameters: List[Pattern], returnType: Expr, expr: Expr, range: FilePosRange) extends GlobalStmt

private def parseSep[T](state: ParserState, element: ParserState => (T, ParserState), sep: String, end: String,
                        desc: String): (List[T], ParserState, FilePosRange) = {
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
    val (expr, rest) = parseExpr(0)(state withTokens exprTokens)
    ((Some(name), expr), rest)
  case exprTokens =>
    val (expr, rest) = parseExpr(0)(state withTokens exprTokens)
    ((None, expr), rest)
}

private def parseStmt(state: ParserState): (Expr, ParserState) = state.tokens match {
  case SymbolToken("}", range) :: rest => (UnitExpr(range), state)
  case _ => parseExpr(0)(state)
}

private def parseExpr(precedence: Int)(state: ParserState): (Expr, ParserState) = state.tokens match {
  case IdenToken(iden, range) :: rest => parseExpr(precedence)(RefExpr(iden, range), state withTokens rest)
  case IntToken(int, range) :: rest => parseExpr(precedence)(IntExpr(int, range), state withTokens rest)
  case SymbolToken("(", startRange) :: exprTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens exprTokens, parseExpr(0), ",", ")", "expression")
    parseExpr(precedence)(exprs match {
      case List() => UnitExpr(startRange to endRange)
      case List(expr) => expr
      case exprList => TupleExpr(exprList, startRange to endRange)
    }, newState)
  case SymbolToken("{", startRange) :: exprTokens =>
    val (body, newState, endRange) = parseSep(state withTokens exprTokens, parseStmt, ";", "}", "expression")
    val lastExpr = body.last
    val exprs = body.dropRight(1)
    parseExpr(precedence)(BlockExpr(exprs, lastExpr, startRange to endRange), newState)
  case KeywordToken("let", startRange) :: patternTokens =>
    val (pattern, newState) = parsePattern(state withTokens patternTokens)
    val (expr, lastState) = parseExpr(0)(newState.expectSymbol("="))
    (LetExpr(pattern, expr, startRange to expr.range), lastState)
  case token :: _ => throw Error.unexpected(token, "expression")
  case _ => throw Error.unexpected("expression", state.file)
}

@tailrec
private def parseExpr(precedence: Int)(expr: Expr, state: ParserState): (Expr, ParserState) = state.tokens match {
  case SymbolToken(".", _) :: IdenToken(op, opRange) :: SymbolToken("(", _) :: argumentTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens argumentTokens, parseArgument, ",", ")", "expression")
    val posArgs = exprs.filter(_._1.isEmpty).map(_._2)
    val keywordArgs = exprs.filter(_._1.nonEmpty).map(t => (t._1.get, t._2))
    parseExpr(precedence)(CallExpr(RefExpr(op, opRange), expr :: posArgs, keywordArgs, expr.range to endRange), newState)
  case SymbolToken(".", _) :: IdenToken(iden, idenRange) :: rest => parseExpr(precedence)(DotExpr(expr, iden, expr.range to idenRange), state withTokens rest)
  case (idenToken: IdenToken) :: rightExprTokens if idenToken.precedence > precedence =>
    val (rightExpr, rest) = parseExpr(idenToken.precedence)(state withTokens rightExprTokens)
    parseExpr(precedence)(CallExpr(RefExpr(idenToken.iden, idenToken.range), List(expr, rightExpr), List(), expr.range to rightExpr.range), rest)
  case SymbolToken("(", _) :: argumentTokens =>
    val (exprs, newState, endRange) = parseSep(state withTokens argumentTokens, parseArgument, ",", ")", "expression")
    val posArgs = exprs.filter(_._1.isEmpty).map(_._2)
    val keywordArgs = exprs.filter(_._1.nonEmpty).map(t => (t._1.get, t._2))
    parseExpr(precedence)(CallExpr(expr, posArgs, keywordArgs, expr.range to endRange), newState)
  case _ => (expr, state)
}

private def parsePattern(state: ParserState): (Pattern, ParserState) = state.tokens match {
  case IdenToken(name, nameRange) :: SymbolToken(":", _) :: rest =>
    val (expr, newState) = parseExpr(0)(state withTokens rest)
    (VarPattern(name, expr, nameRange to expr.range), newState)
  case SymbolToken("(", startRange) :: rest =>
    val (patterns, newState, endRange) = parseSep(state withTokens rest, parsePattern, ",", ")", "pattern")
    (TuplePattern(patterns, startRange to endRange), newState)
  case IdenToken(_, _) :: token :: _ => throw Error.unexpected(token, "':'")
  case token :: _ => throw Error.unexpected(token, "pattern")
  case _ => throw Error.unexpected("pattern", state.file)
}

private def parseGlobalStmt(state: ParserState): (GlobalStmt, ParserState) = state.tokens match {
  case KeywordToken("let", startRange) :: patternTokens =>
    val (pattern, newState) = parsePattern(state withTokens patternTokens)
    val (expr, lastState) = parseExpr(0)(newState.expectSymbol("="))
    (LetGlobalStmt(pattern, expr, startRange to expr.range), lastState)
  case KeywordToken("fun", startRange) :: IdenToken(name, _) :: SymbolToken("(", _) :: parameterTokens =>
    val (parameters, newState, _) = parseSep(state withTokens parameterTokens, parsePattern, ",", ")", "pattern")
    val (returnType, exprState) = parseExpr(0)(newState.expectSymbol(":"))
    val (expr, lastState) = parseExpr(0)(exprState.expectSymbol("=>"))
    (FunGlobalStmt(name, parameters, returnType, expr, startRange to expr.range), lastState)
  case KeywordToken("fun", _) :: IdenToken(_, _) :: token :: _ => throw Error.unexpected(token, "'('")
  case KeywordToken("fun", _) :: IdenToken(_, _) :: _ => throw Error.unexpected("'('", state.file)
  case KeywordToken("fun", _) :: token :: _ => throw Error.unexpected(token, "identifier")
  case KeywordToken("fun", _) :: _ => throw Error.unexpected("identifier", state.file)
  case token :: _ => throw Error.unexpected(token, "let statement or fun statement")
  case _ => throw Error.unexpected("let statement or fun statement", state.file)
}

@tailrec
def parseGlobalStmts(stmts: List[GlobalStmt], state: ParserState): (List[GlobalStmt], ParserState) = state.tokens match {
  case SymbolToken(";", _) :: rest => parseGlobalStmts(stmts, state withTokens rest)
  case tokens if tokens.nonEmpty =>
    val (stmt, newState) = parseGlobalStmt(state)
    parseGlobalStmts(stmt :: stmts, newState)
  case _ => (stmts, state)
}
