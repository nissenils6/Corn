package syn

import core.*
import lex.*

val parseIdenType = for {
  (iden, range) <- parseIden
} yield IdenTypeExpr(iden, range)

lazy val parseParenType: Parser[TypeExpr] = for {
  startRange <- parseSymbol("(")
  elements <- parseType.sepBy(parseSymbol(","))
  endRange <- parseSymbol(")")
} yield elements match {
  case Nil => UnitTypeExpr(startRange | endRange)
  case List(element) => element
  case _ => TupleTypeExpr(elements, startRange | endRange)
}

lazy val parseMutType: Parser[TypeExpr] = for {
  startRange <- parseKeyword("mut")
  typeExpr <- parseParenType <|> parseIdenType <|> parseRefType
} yield MutTypeExpr(typeExpr, startRange | typeExpr.range)

lazy val parseRefType: Parser[TypeExpr] = for {
  startRange <- parseKeyword("@")
  typeExpr <- parseParenType <|> parseIdenType <|> parseMutType
} yield RefTypeExpr(typeExpr, startRange | typeExpr.range)

lazy val parseSingleFunType: Parser[TypeExpr] = for {
  param <- parseIdenType <|> parseRefType <|> parseMutType
  returnType <- parseSymbol("=>") *> parseType
} yield FunTypeExpr(List(param), returnType, param.range | returnType.range)

lazy val parseMultiFunType: Parser[TypeExpr] = for {
  startRange <- parseSymbol("(")
  params <- parseType.sepBy(parseSymbol(","))
  returnType <- parseSymbol(")") *> parseSymbol("=>") *> parseType
}  yield FunTypeExpr(params, returnType, startRange | returnType.range)

lazy val parseType: Parser[TypeExpr] = parseMultiFunType <|> parseParenType <|> parseSingleFunType <|> parseIdenType <|> parseRefType <|> parseMutType
