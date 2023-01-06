package syn

import core.*
import lex.*

val parseVarPattern: Parser[Pattern[AnyVar]] = for {
  (range, iden) <- parseIden
  typeExpr <- (parseSymbol(":") *> parseType).opt
} yield VarPattern(iden, typeExpr, range)

lazy val parseParenPattern: Parser[Pattern[AnyVar]] = for {
  startRange <- parseSymbol("(")
  elements <- parsePattern.sepBy(parseSymbol(","))
  endRange <- parseSymbol(")")
} yield elements match {
  case Nil => assert(false, "Unit patterns not supported yet")
  case List(element) => element
  case _ => TuplePattern(elements, startRange | endRange)
}

lazy val parsePattern: Parser[Pattern[AnyVar]] = parseVarPattern <|> parseParenPattern
