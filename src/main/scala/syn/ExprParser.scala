package syn

import core.*
import lex.*

val parseIntExpr: Parser[Expr] = Parser {
  case IntToken(int, range) :: rest => Right((rest, IntExpr(int, range)))
  case token :: _ => Left(ParserErrorExpected(token.format, Some(token.range), List(s"integer literal")))
  case _ => Left(ParserErrorExpected("End of file", None, List(s"integer literal")))
}

val parseTrue: Parser[Expr] = parseKeyword("true") <#> makeBoolExpr(true)

val parseFalse: Parser[Expr] = parseKeyword("false") <#> makeBoolExpr(false)

val parseIdenExpr: Parser[Expr] = parseIden <#> makeIdenExpr

def parseOperatorExpr(pred: String => Boolean)(prec: Int): Parser[Expr] =
  parseExpr(prec) <#> makeOperatorExpr <*> parseIden.filter(x => pred(x._2), x => ParserErrorExpected(x._2, Some(x._1), List())) <*> parseExpr(prec)

val exprParsers: List[Int => Parser[Expr]] = List(
  parseOperatorExpr(Set("+", "-").contains),
  parseOperatorExpr(Set("*", "/", "%").contains),
    _ => parseIntExpr
)

def parseExpr(prec: Int): Parser[Expr] = Parser.alternative(exprParsers.zipWithIndex.filter(_._2 >= prec).map { case(pf, idx) => () => pf(idx) })

def makeIdenExpr(tuple: (FilePosRange, String)): Expr = IdenExpr(tuple._2, tuple._1)

def makeBoolExpr(value: Boolean)(range: FilePosRange): Expr = BoolExpr(value, range)

def makeOperatorExpr(left: Expr)(tuple: (FilePosRange, String))(right: Expr): Expr = CallExpr(IdenExpr(tuple._2, tuple._1), List(left, right), left.range | right.range)
