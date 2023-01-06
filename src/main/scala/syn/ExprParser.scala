package syn

import core.*
import lex.*

import scala.annotation.tailrec

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
  (range, iden) <- parseIden
} yield IdenExpr(iden, range)

def parseOperatorLeftAssoc(pred: String => Boolean, exprParser: => Parser[Expr]): Parser[Expr] = for {
  (expr :: restExprs, restSeps) <- exprParser.sepBy1(parseIden.filter(x => pred(x._2)))
} yield {
  @tailrec
  def foldExprs(accExpr: Expr, exprs: List[Expr], seps: List[(FilePosRange, String)]): Expr = (exprs, seps) match {
    case (Nil, Nil) => accExpr
    case (expr :: restExprs, (range, op) :: restSeps) => foldExprs(CallExpr(IdenExpr(op, range), List(accExpr, expr), accExpr.range | expr.range), restExprs, restSeps)
    case _ => assert(false, "unreachable")
  }

  foldExprs(expr, restExprs, restSeps)
}

def parseOperatorRightAssoc(pred: String => Boolean, exprParser: => Parser[Expr]): Parser[Expr] = for {
  (restExprs, restSeps) <- exprParser.sepBy1(parseIden.filter(x => pred(x._2)))
} yield {
  def foldExprs(exprs: List[Expr], seps: List[(FilePosRange, String)]): Expr = (exprs, seps) match {
    case (List(expr), Nil) => expr
    case (left :: restExprs, (range, op) :: restSeps) =>
      val right = foldExprs(restExprs, restSeps)
      CallExpr(IdenExpr(op, range), List(left, right), left.range | right.range)
    case _ => assert(false, "unreachable")
  }

  foldExprs(restExprs, restSeps)
}

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

lazy val parseIdenOperator: Parser[Expr] = parseOperatorLeftAssoc(isIdenOp, parseSymOperator)
lazy val parseSymOperator: Parser[Expr] = parseOperatorLeftAssoc(isNonSpecialOp, parseColon)
lazy val parseColon: Parser[Expr] = parseOperatorRightAssoc(isColonOp, parseBitwise)
lazy val parseBitwise: Parser[Expr] = parseOperatorLeftAssoc(isBitwiseOp, parseShift)
lazy val parseShift: Parser[Expr] = parseOperatorLeftAssoc(isShiftOp, parseAddition)
lazy val parseAddition: Parser[Expr] = parseOperatorLeftAssoc(isAddOp, parseMultiplication)
lazy val parseMultiplication: Parser[Expr] = parseOperatorLeftAssoc(isMultOp, parseHighPrec)

lazy val parseDot: Parser[Expr] = for {
  expr <- parseAtom
  _ <- parseSymbol(".")
  (range, iden) <- parseIden
} yield DotExpr(expr, iden, expr.range | range)

lazy val parseFunCall: Parser[Expr] = for {
  expr <- parseAtom
  _ <- parseSymbol("(")
  args <- parseExpr1.sepBy(parseSymbol(","))
  endRange <- parseSymbol(")")
} yield CallExpr(expr, args, expr.range | endRange)

lazy val parseHighPrec: Parser[Expr] = parseDot <|> parseFunCall <|> parseAtom

lazy val parseParenExpr: Parser[Expr] = parseSymbol("(") *> parseExpr1 <* parseSymbol(")")

lazy val parseFunExpr: Parser[Expr] = for {
  startRange <- parseSymbol("(")
  params <- parsePattern1.sepBy(parseSymbol(","))
  _ <- parseSymbol(")")
  _ <- parseType.opt
  _ <- parseSymbol("=>").opt
  expr <- parseExpr1
} yield FunExpr(params, None /*returnType*/, expr, startRange | expr.range)

lazy val parseIfExpr: Parser[Expr] = for {
  startRange <- parseKeyword("if")
  condition <- parseExpr1
  _ <- parseKeyword("then").opt
  ifBlock <- parseExpr1
  elseBlock <- (parseKeyword("else") *> parseExpr1).opt
} yield elseBlock match {
  case Some(elseBlock) =>  IfExpr(condition, ifBlock, elseBlock, startRange | elseBlock.range)
  case None => IfExpr(condition, ifBlock, UnitExpr(ifBlock.range.after), startRange | ifBlock.range)
}

lazy val parseAtom: Parser[Expr] = parseFunExpr <|> parseParenExpr <|> parseIntExpr <|> parseTrueExpr <|> parseFalseExpr <|> parseIdenExpr <|> parseIfExpr

lazy val parseExpr1 = parseIdenOperator
