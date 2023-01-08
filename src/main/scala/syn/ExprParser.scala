package syn

import core.*
import lex.*

import scala.annotation.tailrec

def isIdenOp(s: String) = s.exists {
  case IdenChar(_) => true
  case _ => false
}

def isNonSpecialOp(s: String) = !(isColonOp(s) || isBitwiseOp(s) || isShiftOp(s) || isAddOp(s) || isMultOp(s) || isIdenOp(s))

def isColonOp(s: String) = s.contains(':')

def isBitwiseOp(s: String) = s match {
  case "&" | "|" | "^" => true
  case _ => false
}

def isShiftOp(s: String) = s.forall(_ == '>') || s.forall(_ == '<')

def isAddOp(s: String) = s match {
  case "+" | "-" => true
  case _ => false
}

def isMultOp(s: String) = s match {
  case "*" | "/" | "%" => true
  case _ => false
}

def parseOperatorLeftAssoc(sepParser: Parser[(String, FilePosRange)], exprParser: => Parser[Expr]): Parser[Expr] = for {
  (expr :: restExprs, restSeps) <- exprParser.sepByC(sepParser)
} yield {
  @tailrec
  def foldExprs(accExpr: Expr, exprs: List[Expr], seps: List[(String, FilePosRange)]): Expr = (exprs, seps) match {
    case (Nil, Nil) => accExpr
    case (expr :: restExprs, (op, range) :: restSeps) => foldExprs(CallExpr(IdenExpr(op, range), List(accExpr, expr), accExpr.range | expr.range), restExprs, restSeps)
    case _ => assert(false, "unreachable")
  }

  foldExprs(expr, restExprs, restSeps)
}

def parseOperatorRightAssoc(sepParser: Parser[(String, FilePosRange)], exprParser: => Parser[Expr]): Parser[Expr] = for {
  (restExprs, restSeps) <- exprParser.sepByC(sepParser)
} yield {
  def foldExprs(exprs: List[Expr], seps: List[(String, FilePosRange)]): Expr = (exprs, seps) match {
    case (List(expr), Nil) => expr
    case (left :: restExprs, (op, range) :: restSeps) =>
      val right = foldExprs(restExprs, restSeps)
      CallExpr(IdenExpr(op, range), List(left, right), left.range | right.range)
    case _ => assert(false, "unreachable")
  }

  foldExprs(restExprs, restSeps)
}

lazy val parseIdenOperator: Parser[Expr] = parseOperatorLeftAssoc(parseIden(isIdenOp), parseSymOperator)
lazy val parseSymOperator: Parser[Expr] = parseOperatorLeftAssoc(parseIden(isNonSpecialOp), parseColon)
lazy val parseColon: Parser[Expr] = parseOperatorRightAssoc(parseIden(isColonOp), parseBitwise)
lazy val parseBitwise: Parser[Expr] = parseOperatorLeftAssoc(parseIden(isBitwiseOp), parseShift)
lazy val parseShift: Parser[Expr] = parseOperatorLeftAssoc(parseIden(isShiftOp), parseAddition)
lazy val parseAddition: Parser[Expr] = parseOperatorLeftAssoc(parseIden(isAddOp), parseMultiplication)
lazy val parseMultiplication: Parser[Expr] = parseOperatorLeftAssoc(parseIden(isMultOp), parseUnary)

lazy val parseRef: Parser[Expr] = for {
  startRange <- parseSymbol("@")
  expr <- parseHighPrec
} yield RefExpr(expr, startRange | expr.range)

lazy val parseVal: Parser[Expr] = for {
  startRange <- parseSymbol("!")
  expr <- parseHighPrec
} yield ValExpr(expr, startRange | expr.range)

lazy val parseUnary: Parser[Expr] = parseRef <|> parseVal <|> parseHighPrec

lazy val parseDot: Parser[Expr] = for {
  expr <- parseAtom
  _ <- parseSymbol(".")
  (iden, range) <- parseIden
} yield DotExpr(expr, iden, expr.range | range)

lazy val parseFunCall: Parser[Expr] = for {
  expr <- parseAtom
  _ <- parseSymbol("(")
  args <- parseExpr.sepBy(parseSymbol(","))
  endRange <- parseSymbol(")")
} yield CallExpr(expr, args, expr.range | endRange)

lazy val parseHighPrec: Parser[Expr] = parseDot <|> parseFunCall <|> parseAtom

lazy val parseFunExpr: Parser[Expr] = for {
  startRange <- parseSymbol("(")
  params <- parsePattern.sepBy(parseSymbol(","))
  _ <- parseSymbol(")")
  returnType <- (parseSymbol(":") *> parseType).opt
  _ <- parseSymbol("=>").opt
  expr <- parseExpr
} yield FunExpr(params.asInstanceOf[List[Pattern[Var]]], returnType, expr, startRange | expr.range)

lazy val parseParenExpr: Parser[Expr] = for {
  startRange <- parseSymbol("(")
  exprs <- parseExpr.sepBy(parseSymbol(","))
  endRange <- parseSymbol(")")
} yield exprs match {
  case Nil => UnitExpr(startRange | endRange)
  case List(expr) => expr
  case _ => TupleExpr(exprs, startRange | endRange)
}

lazy val parseBlockExpr: Parser[Expr] = for {
  startRange <- parseSymbol("{")
  stmts <- parseStmt.many
  expr <- parseExpr.opt
  endRange <- parseSymbol("}")
} yield BlockExpr(stmts, expr.getOrElse(UnitExpr(endRange)), startRange | endRange)

val parseIntExpr: Parser[Expr] = Parser {
  case IntToken(int, range) :: rest => Right((rest, IntExpr(int, range)))
  case token :: _ => Left(ParserErrorExpected(token.format, Some(token.range), List(s"integer literal")))
  case _ => Left(ParserErrorExpected("End of file", None, List(s"integer literal")))
}

val parseTrueExpr: Parser[Expr] = for {
  range <- parseKeyword("true")
} yield BoolExpr(true, range)

val parseFalseExpr: Parser[Expr] = for {
  range <- parseKeyword("false")
} yield BoolExpr(false, range)

val parseIdenExpr: Parser[Expr] = for {
  (iden, range) <- parseIden
} yield IdenExpr(iden, range)

lazy val parseIfExpr: Parser[Expr] = for {
  startRange <- parseKeyword("if")
  condition <- parseExpr
  _ <- parseKeyword("then").opt
  ifBlock <- parseExpr
  elseBlock <- (parseKeyword("else") *> parseExpr).opt
} yield elseBlock match {
  case Some(elseBlock) => IfExpr(condition, ifBlock, elseBlock, startRange | elseBlock.range)
  case None => IfExpr(condition, ifBlock, UnitExpr(ifBlock.range.after), startRange | ifBlock.range)
}

lazy val parseAtom: Parser[Expr] = parseFunExpr <|> parseParenExpr <|> parseBlockExpr <|> parseIntExpr <|> parseTrueExpr <|> parseFalseExpr <|> parseIdenExpr <|> parseIfExpr

lazy val parseExpr = parseIdenOperator
