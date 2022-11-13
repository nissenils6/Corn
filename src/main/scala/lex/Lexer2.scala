package lex

import core.*

import scala.annotation.tailrec

abstract class Lexer2State

case object BlankState extends Lexer2State
case class IdenState(isSym: Boolean, iden: List[Char], range: FilePosRange) extends Lexer2State
case class NumberState(num: List[Char], range: FilePosRange) extends Lexer2State
case class StringState(isChar: Boolean, string: List[Char], range: FilePosRange) extends Lexer2State
case class CommentState(isBlock: Boolean, string: List[Char], range: FilePosRange) extends Lexer2State

object IdenStartChar {
  def unapply(char: Char): Option[Char] = Some(char).filter(isIdenStart)
}

object IdenChar {
  def unapply(char: Char): Option[Char] = Some(char).filter(isIden)
}

object IdenSymChar {
  def unapply(char: Char): Option[Char] = Some(char).filter(idenSymbols.contains)
}

object SymChar {
  def unapply(char: Char): Option[Char] = Some(char).filter(specialSymbols.contains)
}

object NumberStartChar {
  def unapply(char: Char): Option[Char] = Some(char).filter(_.isDigit)
}

object NumberChar {
  def unapply(char: Char): Option[Char] = Some(char).filter(c => c.isDigit || c == '_' || c == '.')
}

object WhitespaceChar {
  def unapply(char: Char): Option[Char] = Some(char).filter(_.isWhitespace)
}

object EscapedChar {
  def unapply(char: Char): Option[Char] = escapedChars.get(char)
}

object SymbolString {
  def unapply(string: String): Option[String] = Some(string).filter(symbols.contains)
}

object KeywordString {
  def unapply(string: String): Option[String] = Some(string).filter(keywords.contains)
}

def tokenize2(file: File): List[Token] = {
  object Range {
    def unapply(int: Int): Option[FilePosRange] = Some(FilePosRange(int, int + 1, file))
  }

  @tailrec
  def lex(state: Lexer2State, chars: List[(Char, Int)], tokens: List[Token], errors: List[ErrorComponent]): (List[Token], List[ErrorComponent]) = (state, chars) match {
    case (BlankState, List()) => (tokens, errors)
    case (BlankState, (WhitespaceChar(_), _) :: rest) => lex(state, rest, tokens, errors)

    case (BlankState, (SymChar(char), Range(range)) :: rest) => lex(BlankState, rest, SymbolToken(char.toString, range) :: tokens, errors)
    case (BlankState, ('-', Range(start)) :: (NumberStartChar(char), _) :: rest) =>
      lex(NumberState(List(char, '-'), start), rest, tokens, errors)
    case (BlankState, (IdenStartChar(char), Range(start)) :: rest) => lex(IdenState(false, List(char), start), rest, tokens, errors)
    case (BlankState, (IdenSymChar(char), Range(start)) :: rest) => lex(IdenState(true, List(char), start), rest, tokens, errors)
    case (BlankState, (NumberStartChar(char), Range(start)) :: rest) => lex(NumberState(List(char), start), rest, tokens, errors)
    case (BlankState, ('"', Range(start)) :: rest) => lex(StringState(false, List(), start), rest, tokens, errors)
    case (BlankState, ('\'', Range(start)) :: rest) => lex(StringState(true, List(), start), rest, tokens, errors)

    case (IdenState(false, iden, range), (IdenChar(char), Range(charRange)) :: rest) =>
      lex(IdenState(false, char :: iden, range to charRange), rest, tokens, errors)
    case (IdenState(true, iden, range), (IdenSymChar(char), Range(charRange)) :: rest) =>
      lex(IdenState(true, char :: iden, range to charRange), rest, tokens, errors)
    case (IdenState(_, iden, range), rest) =>
      lex(BlankState, rest, (iden.reverse.mkString match {
        case SymbolString(symbol) => SymbolToken(symbol, range)
        case KeywordString(keyword) => KeywordToken(keyword, range)
        case iden => IdenToken(iden, range)
      }) :: tokens, errors)

    case (NumberState(num, range), (NumberChar(char), Range(charRange)) :: rest) =>
      lex(NumberState(char :: num, range to charRange), rest, tokens, errors)
    case (NumberState(num, range), rest) => (num.filterNot(_ == '_'), num.count(_ == '.')) match {
      case (num, 0) => lex(BlankState, rest, IntToken(num.reverse.mkString.toLong, range) :: tokens, errors)
      case (num, 1) => lex(BlankState, rest, FloatToken(num.reverse.mkString.toDouble, range) :: tokens, errors)
      case (_, _) => lex(BlankState, rest, tokens, ErrorComponent(range, Some("Floating point literals cannot contain multiple decimal points")) :: errors)
    }

    case (StringState(false, string, range), ('"', Range(end)) :: rest) =>
      lex(BlankState, rest, StringToken(string.reverse.mkString, range to end) :: tokens, errors)
    case (StringState(true, List(char), range), ('\'', Range(end)) :: rest) =>
      lex(BlankState, rest, CharToken(char, range to end) :: tokens, errors)
    case (StringState(true, _, range), ('\'', Range(end)) :: rest) =>
      lex(BlankState, rest, tokens, ErrorComponent(range to end, Some("Character literals must contain exactly one character")) :: errors)
    case (StringState(isChar, string, range), ('\\', _) :: (EscapedChar(char), _) :: rest) =>
      lex(StringState(isChar, char :: string, range), rest, tokens, errors)
    case (StringState(isChar, string, range), ('\\', _) :: (char, Range(charRange)) :: rest) =>
      lex(StringState(isChar, char :: string, range), rest, tokens, ErrorComponent(charRange, Some(s"Inescapable character '$char'")) :: errors)
    case (StringState(isChar, string, range), (char, _) :: rest) =>
      lex(StringState(isChar, char :: string, range), rest, tokens, errors)
    case (StringState(isChar, _, range), List()) =>
      (tokens, ErrorComponent(range.after, Some(s"Unexpected end of file while parsing ${if isChar then "character" else "string"} literal")) :: errors)
  }

  val (tokens, errors) = lex(BlankState, file.source.toList.zipWithIndex, List(), List())
  if (errors.nonEmpty) throw Error(Error.LEXICAL, file, errors.reverse)
  tokens.reverse
}
