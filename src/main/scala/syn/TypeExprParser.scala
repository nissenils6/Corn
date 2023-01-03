package syn

import core.*
import lex.*

val parseIdenType = parseIden <#> makeIdenType

lazy val parseParenType: Parser[TypeExpr] = parseSymbol("(") <#> makeParenType <*> parseType.sepBy(parseSymbol(",")) <*> parseSymbol(")")

lazy val parseMutType: Parser[TypeExpr] = parseKeyword("mut") <#> makeMutType <*> (parseParenType <|> parseIdenType <|> parseRefType)

lazy val parseRefType: Parser[TypeExpr] = parseSymbol("@") <#> makeRefType <*> (parseParenType <|> parseIdenType <|> parseMutType)

lazy val parseSingleFunType: Parser[TypeExpr] = (parseIdenType <|> parseRefType <|> parseMutType) <#> makeSingleFunType <*> (parseSymbol("=>") *> parseType)

lazy val parseMultiFunType: Parser[TypeExpr] = parseSymbol("(") <#> makeMultiFunType <*> parseType.sepBy(parseSymbol(",")) <*> (parseSymbol(")") *> parseSymbol("=>") *> parseType)

lazy val parseType: Parser[TypeExpr] = parseMultiFunType <|> parseParenType <|> parseSingleFunType <|> parseIdenType <|> parseRefType <|> parseMutType

def makeIdenType(tuple: (FilePosRange, String)): TypeExpr = IdenTypeExpr(tuple._2, tuple._1)

def makeParenType(startRange: FilePosRange)(elements: List[TypeExpr])(endRange: FilePosRange): TypeExpr = elements match {
  case List() => UnitTypeExpr(startRange | endRange)
  case List(element) => element
  case _ => TupleTypeExpr(elements, startRange | endRange)
}

def makeMutType(startRange: FilePosRange)(typeExpr: TypeExpr): TypeExpr = MutTypeExpr(typeExpr, startRange | typeExpr.range)

def makeRefType(startRange: FilePosRange)(typeExpr: TypeExpr): TypeExpr = RefTypeExpr(typeExpr, startRange | typeExpr.range)

def makeSingleFunType(param: TypeExpr)(returnType: TypeExpr): TypeExpr = FunTypeExpr(List(param), returnType, param.range | returnType.range)

def makeMultiFunType(startRange: FilePosRange)(params: List[TypeExpr])(returnType: TypeExpr): TypeExpr = FunTypeExpr(params, returnType, startRange | returnType.range)
