package syn

import core.*
import lex.*

import scala.annotation.targetName
import scala.compiletime.ops.{int, string}

abstract class ParserError {
  @targetName("combine")
  def |(e: ParserError): ParserError = (this, e) match {
    case (ParserErrorExpected(found, expected1), ParserErrorExpected(_, expected2)) => ParserErrorExpected(found, expected1.concat(expected2))
  }
}

case class ParserErrorExpected(found: String, expected: List[String]) extends ParserError

case class Parser[+R](t: List[Token] => Either[ParserError, (List[Token], R)]) {
  @inline
  @targetName("penetrate")
  def <#>[R2](f: R => R2): Parser[R2] = Parser { s =>
    t(s).map { case (ns, r) =>
      (ns, f(r))
    }
  }

  @inline
  @targetName("sequenceLeft")
  def <*[R2](p: Parser[R2]): Parser[R] = Parser { s =>
    t(s).flatMap { case (ns, a) =>
      p.t(ns).map { case (nns, _) =>
        (nns, a)
      }
    }
  }

  @inline
  @targetName("sequenceRight")
  def *>[R2](p: Parser[R2]): Parser[R2] = Parser { s =>
    t(s).flatMap { case (ns, _) =>
      p.t(ns).map { case (nns, b) =>
        (nns, b)
      }
    }
  }

  def opt: Parser[Option[R]] = Parser { s =>
    t(s) match {
      case Right((ns, result)) => Right((ns, Some(result)))
      case Left(_) => Right((s, None))
    }
  }

  def many: Parser[List[R]] = Parser { s =>
    var cs = s
    var list: List[R] = List.empty
    var exit = false
    while (!exit) {
      t(cs) match {
        case Right((ns, result)) =>
          list = result :: list
          cs = ns
        case Left(_) =>
          exit = true
      }
    }
    Right((cs, list.reverse))
  }

  def some: Parser[List[R]] = Parser { s =>
    t(s) match {
      case Right((ns, result)) =>
        var cs = ns
        var list: List[R] = List(result)
        var exit = false
        while (!exit) {
          t(cs) match {
            case Right((ns, result)) =>
              list = result :: list
              cs = ns
            case Left(_) =>
              exit = true
          }
        }
        Right((cs, list.reverse))
      case Left(e) => Left(e)
    }
  }

  def sepBy(p: Parser[Any]): Parser[List[R]] = Parser { s =>
    t(s) match {
      case Right((ns, result)) =>
        var cs = ns
        var list: List[R] = List(result)
        var error: Option[ParserError] = None
        var exit = false
        while (!exit) {
          p.t(cs) match {
            case Right((ss, _)) => t(ss) match {
              case Right((ns, result)) =>
                list = result :: list
                cs = ns
              case Left(e) =>
                error = Some(e)
                exit = true
            }
            case Left(_) => exit = true
          }
        }
        error.toLeft((cs, list.reverse))
      case Left(e) => Left(e)
    }
  }
}

extension[R] (parser: => Parser[R]) {
  @inline
  @targetName("alternative")
  def <|>[R2 >: R, R1 <: R2](p: => Parser[R1]): Parser[R2] = Parser { s =>
    parser.t(s) match {
      case Left(e1) => p.t(s) match {
        case Left(e2) => Left(e1 | e2)
        case result => result
      }
      case result => result
    }
  }
}

extension[A, B] (parser: Parser[A => B]) {
  @inline
  @targetName("sequence")
  def <*>(p: Parser[A]): Parser[B] = Parser { s =>
    parser.t(s).flatMap { case (ns, f) =>
      p.t(ns).map { case (nns, a) =>
        (nns, f(a))
      }
    }
  }
}

def parseKeyword(keyword: String): Parser[FilePosRange] = Parser {
  case KeywordToken(kw, range) :: rest if keyword == kw => Right((rest, range))
  case token :: _ => Left(ParserErrorExpected(token.toString, List(s"'$keyword'")))
  case _ => Left(ParserErrorExpected("End of file", List(s"'$keyword'")))
}

def parseSymbol(symbol: String): Parser[FilePosRange] = Parser {
  case SymbolToken(sym, range) :: rest if symbol == sym => Right((rest, range))
  case token :: _ => Left(ParserErrorExpected(token.toString, List(s"'$symbol'")))
  case _ => Left(ParserErrorExpected("End of file", List(s"'$symbol'")))
}

val parseInt = Parser {
  case IntToken(int, range) :: rest => Right((rest, IntExpr(int, range)))
  case token :: _ => Left(ParserErrorExpected(token.toString, List(s"integer literal")))
  case _ => Left(ParserErrorExpected("End of file", List(s"integer literal")))
}

val parseBool = Parser {
  case KeywordToken("true", range) :: rest => Right((rest, BoolExpr(true, range)))
  case KeywordToken("false", range) :: rest => Right((rest, BoolExpr(false, range)))
  case token :: _ => Left(ParserErrorExpected(token.toString, List(s"string literal")))
  case _ => Left(ParserErrorExpected("End of file", List(s"string literal")))
}

val parseIden = Parser {
  case IdenToken(iden, range) :: rest => Right((rest, IdenExpr(iden, range)))
  case token :: _ => Left(ParserErrorExpected(token.toString, List(s"identifier")))
  case _ => Left(ParserErrorExpected("End of file", List(s"identifier")))
}

val parseIdenType = Parser {
  case IdenToken(iden, range) :: rest => Right((rest, IdenTypeExpr(iden, range)))
  case token :: _ => Left(ParserErrorExpected(token.toString, List(s"identifier")))
  case _ => Left(ParserErrorExpected("End of file", List(s"identifier")))
}

lazy val parseTupleType: Parser[TypeExpr] = parseSymbol("(") <#> makeTupleType <*> parseTypeExpr.sepBy(parseSymbol(",")) <*> parseSymbol(")")

lazy val parseMutType: Parser[TypeExpr] = parseKeyword("mut") <#> makeMutType <*> (parseTupleType <|> parseIdenType <|> parseRefType)

lazy val parseRefType: Parser[TypeExpr] = parseSymbol("&") <#> makeRefType <*> (parseTupleType <|> parseIdenType <|> parseMutType)

lazy val parseSingleFunType: Parser[TypeExpr] = (parseIdenType <|> parseRefType <|> parseMutType) <#> makeSingleFunType <*> (parseSymbol("=>") *> parseTypeExpr)

lazy val parseMultiFunType: Parser[TypeExpr] = parseSymbol("(") <#> makeMultiFunType <*> parseTypeExpr.sepBy(parseSymbol(",")) <*> (parseSymbol("(") *> parseSymbol("=>") *> parseTypeExpr)

lazy val parseTypeExpr: Parser[TypeExpr] = parseMultiFunType <|> parseTupleType <|> parseSingleFunType <|> parseIdenType <|> parseRefType <|> parseMutType

def makeTupleType(startRange: FilePosRange)(elements: List[TypeExpr])(endRange: FilePosRange) = elements match {
  case List() => UnitTypeExpr(startRange | endRange)
  case List(element) => element
  case _ => TupleTypeExpr(elements, startRange | endRange)
}

def makeMutType(startRange: FilePosRange)(typeExpr: TypeExpr) = MutTypeExpr(typeExpr, startRange | typeExpr.range)

def makeRefType(startRange: FilePosRange)(typeExpr: TypeExpr) = RefTypeExpr(typeExpr, startRange | typeExpr.range)

def makeSingleFunType(param: TypeExpr)(returnType: TypeExpr) = FunTypeExpr(List(param), returnType, param.range | returnType.range)

def makeMultiFunType(startRange: FilePosRange)(params: List[TypeExpr])(returnType: TypeExpr) = FunTypeExpr(params, returnType, startRange | returnType.range)

def testParsingLibrary() = {
  val tokens = tokenize(File("src/main/scala/syn/TestFile"))
  println(tokens.mkString(" "))
  parseTypeExpr.t(tokens) match {
    case Right((remainingTokens, typeExpr)) => println(typeExpr)
    case Left(ParserErrorExpected(found, expected)) => println(s"Unexpected $found expected ${expected.toSet.mkString(", ")}")
  }
}
