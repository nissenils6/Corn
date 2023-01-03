package syn

import core.*
import lex.*

import scala.collection.mutable
import scala.annotation.targetName
import scala.compiletime.ops.{int, string}

abstract class ParserError {
  @targetName("combine")
  def |(e: ParserError): ParserError = (this, e) match {
    case (ParserErrorExpected(_, _, expected1), ParserErrorExpected(found, range, expected2)) => ParserErrorExpected(found, range, expected1.concat(expected2))
  }
}

case class ParserErrorExpected(found: String, range: Option[FilePosRange], expected: List[String]) extends ParserError

case class Parser[+R](t: List[Token] => Either[ParserError, (List[Token], R)]) {
  @inline
  @targetName("map")
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
      case Left(e) => Right((s, List()))
    }
  }
  
  def filter(f: R => Boolean, e: R => ParserError) = Parser { s =>
    t(s) match {
      case Right((ns, r)) if f(r) => Right((ns, r))
      case Right((_, r)) => Left(e(r))
      case Left(e) => Left(e)
    }
  }
}

object Parser {
  def alternative[R](ps: List[() => Parser[R]]): Parser[R] = Parser { s =>
    val errors: mutable.Buffer[ParserError] = mutable.Buffer.empty
    var result: Option[(List[Token], R)] = None
    ps.find { p =>
      p().t(s) match {
        case Right((ns, r)) =>
          result = Some((ns, r))
          true
        case Left(error) =>
          errors.append(error)
          false
      }
    }
    result.toRight(errors.foldRight(ParserErrorExpected("", None, List.empty) : ParserError)(_ | _))
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
  case token :: _ => Left(ParserErrorExpected(token.format, Some(token.range), List(s"'$keyword'")))
  case _ => Left(ParserErrorExpected("End of file", None, List(s"'$keyword'")))
}

def parseSymbol(symbol: String): Parser[FilePosRange] = Parser {
  case SymbolToken(sym, range) :: rest if symbol == sym => Right((rest, range))
  case token :: _ => Left(ParserErrorExpected(token.format, Some(token.range), List(s"'$symbol'")))
  case _ => Left(ParserErrorExpected("End of file", None, List(s"'$symbol'")))
}

val parseIden: Parser[(FilePosRange, String)] = Parser {
  case IdenToken(iden, range) :: rest => Right((rest, (range, iden)))
  case token :: _ => Left(ParserErrorExpected(token.format, Some(token.range), List(s"identifier")))
  case _ => Left(ParserErrorExpected("End of file", None, List(s"identifier")))
}

def testParsingLibrary(): Unit = {
  val file = File("src/main/scala/syn/TestFile")
  val tokens = tokenize(file)
  parseExpr(0).t(tokens) match {
    case Right((_, expr)) => println(expr.format(0))
    case Left(ParserErrorExpected(found, range, expected)) => println(s"Unexpected $found at ${range.getOrElse(file.lastRange)} expected ${expected.toSet.mkString(", ")}")
    case _ => ???
  }
}
