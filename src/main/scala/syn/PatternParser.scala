package syn

import core.*
import lex.*

val parseVarPattern: Parser[Pattern] = for {
  (range, iden) <- parseIden
  _ <- (parseSymbol(":") *> parseType).opt
} yield VarPattern(iden, None /*typeExpr*/ , range)

lazy val parseParenPattern: Parser[Pattern] = for {
  startRange <- parseSymbol("(")
  elements <- parsePattern1.sepBy(parseSymbol(","))
  endRange <- parseSymbol(")")
} yield elements match {
  case Nil => assert(false, "Unit patterns not supported yet")
  case List(element) => element
  case _ => TuplePattern(elements, startRange | endRange)
}

lazy val parsePattern1: Parser[Pattern] = parseVarPattern <|> parseParenPattern
