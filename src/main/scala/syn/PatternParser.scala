package syn

import core.*
import lex.*

val parseVarPattern: Parser[Pattern] = parseIden <#> makeVarPattern <*> (parseSymbol(":") *> parseType).opt

lazy val parseParenPattern: Parser[Pattern] = parseSymbol("(") <#> makeParenPattern <*> parsePattern.sepBy(parseSymbol(",")) <*> parseSymbol(")")

lazy val parsePattern: Parser[Pattern] = parseVarPattern <|> parseParenPattern

def makeVarPattern(tuple: (FilePosRange, String))(typeExpr: Option[TypeExpr]): Pattern = VarPattern(tuple._2, None /*typeExpr*/, tuple._1)

def makeParenPattern(startRange: FilePosRange)(elements: List[Pattern])(endRange: FilePosRange): Pattern = elements match {
  case List() => assert(false, "Unit patterns not supported yet")
  case List(element) => element
  case _ => TuplePattern(elements, startRange | endRange)
}
