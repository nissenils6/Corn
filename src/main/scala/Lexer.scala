import scala.annotation.{targetName, unused}

type LexerResult = Either[Error, Token]

extension (result: LexerResult) {
  def isIgnored = result match {
    case Right(WhitespaceToken(_, _)) => true
    case Right(CommentToken(_, _)) => true
    case _ => false
  }
}

case class LexerState(chars: List[Char], pos: FilePos)

abstract class Token() {
  def range: FilePosRange

  override def toString: String = this match {
    case IdenToken(iden, _) => iden

    case SymbolToken(symbol, _) => symbol
    case KeywordToken(keyword, _) => keyword

    case IntToken(int, _) => int.toString
    case FloatToken(float, _) => float.toString
    case CharToken(char, _) => s"'${unescapeChar(char)}'"
    case StringToken(string, _) => s"\"${string.flatMap(unescapeChar)}\""

    case WhitespaceToken(_, _) => ""
    case CommentToken(comment, _) => comment
  }

  def isSymbol(symbol: String): Boolean = this.isInstanceOf[SymbolToken] && this.asInstanceOf[SymbolToken].symbol == symbol
}

case class IdenToken(iden: String, range: FilePosRange) extends Token {
  val precedence: Int = if (isSym(iden.head)) {
    iden match {
      case "*" | "/" | "%" => 8
      case "+" | "-" => 7
      case ">>" | "<<" => 6
      case ">" | "<" | ">=" | "<=" => 5
      case "==" | "!=" => 4
      case "|" | "&" | "^" => 3
      case _ => 2
    }
  } else 1
}

case class SymbolToken(symbol: String, range: FilePosRange) extends Token

case class KeywordToken(keyword: String, range: FilePosRange) extends Token

case class IntToken(int: Int, range: FilePosRange) extends Token

case class FloatToken(int: Float, range: FilePosRange) extends Token

case class CharToken(char: Char, range: FilePosRange) extends Token

case class StringToken(string: String, range: FilePosRange) extends Token

case class WhitespaceToken(string: String, range: FilePosRange) extends Token

case class CommentToken(comment: String, range: FilePosRange) extends Token

private val idenSymbols = "+-*/%<>=!&|^~?:".toSet
private val escapedChars = Map('t' -> '\t', 'b' -> '\b', 'n' -> '\n', 'r' -> '\r', '\'' -> '\'', '"' -> '"', '\\' -> '\\')
private val escapedCharsInverted = escapedChars.map(_.swap)
private val symbols = Set(":", "::", "...", "=>", "=")
private val specialSymbols = "()[]{}.,;".toSet
private val keywords = Set("let", "fun")
private val nextChar = State[LexerState, (Char, FilePos)](state =>
  ((state.chars.head, state.pos), LexerState(state.chars.tail, state.pos + 1))
)

private val nextCharOption = State[LexerState, Option[(Char, FilePos)]] {
  case LexerState(List(), filePos) => (None, LexerState(List(), filePos))
  case LexerState(chars, filePos) => (Some((chars.head, filePos)), LexerState(chars.tail, filePos + 1))
}

private val nextEscapedChar = nextChar.flatMap {
  case ('\\', filePos) => nextChar.map(t => (escapeChar(t._1), filePos until t._2))
  case (c, filePos) => State.insert((c, filePos.range))
}

private val nextElement: State[LexerState, LexerResult] = nextChar.flatMap {
  case (c, pos) if specialSymbols.contains(c) => State.insert(Right(SymbolToken(c.toString, pos.range)))
  case ('\'', posStart) => for {
    chars <- nextString('\'')
    quote <- nextCharOption
  } yield if (quote.nonEmpty && quote.get._1 == '\'') {
    if (chars._1.length == 1) {
      Right(CharToken(chars._1.head, chars._2))
    } else {
      Left(Error.lexical("Character literals must be exactly one character long", posStart until quote.get._2))
    }
  } else {
    Left(Error.lexical("Unexpected end of file while parsing character literal", posStart until chars._2))
  }
  case ('"', posStart) => for {
    chars <- nextString('"')
    terminator <- nextCharOption
  } yield if (terminator.nonEmpty && terminator.get._1 == '"') {
    Right(StringToken(chars._1.mkString, chars._2))
  } else {
    Left(Error.lexical("Unexpected end of file while parsing string literal", posStart until chars._2))
  }
  case c if isWhitespace(c._1) => next(isWhitespace, c).map(string => Right(WhitespaceToken(string._1, c._2 until string._2)))
  case c if isIdenStart(c._1) => next(isIden, c).flatMap {
    case (keyword, range) if keywords.contains(keyword) => State.insert(Right(KeywordToken(keyword, range)))
    case (iden, range) => State.insert(Right(IdenToken(iden, range)))
  }
  case c if isSym(c._1) => next(isSym, c).flatMap {
    case ("//", pos) => next(_ != '\n', ("//", pos)).map(comment => Right(CommentToken(comment._1, comment._2)))
    case symbol if symbols.contains(symbol._1) => State.insert(Right(SymbolToken(symbol._1, symbol._2)))
    case iden => State.insert(Right(IdenToken(iden._1, iden._2)))
  }
  case c if isNumberStart(c._1) => next(isNumber, c).map(string => string._1.count(_ == '.') match {
    case 0 => Right(IntToken(string._1.toInt, string._2))
    case 1 => Right(FloatToken(string._1.toFloat, string._2))
    case _ => Left(Error.lexical(s"Floating point literals cannot contain multiple decimal points", string._2))
  })
  case c => next(isUnsupported, c).map(string => Left(Error.lexical(s"Unsupported string of characters", string._2)))
}

def escapeChar(char: Char): Char = escapedChars(char)
def unescapeChar(char: Char): String = if (escapedCharsInverted.contains(char)) {
  "\\" + escapedCharsInverted(char)
} else {
  "" + char
}

private def isIdenStart(char: Char) = char.isLetter || char == '_'
private def isNumberStart(char: Char) = char.isDigit
private def isNumber(char: Char) = char.isDigit || char == '.' || char == '_'
private def isUnsupported(char: Char) = !isSym(char) && !isIden(char) && !isWhitespace(char) && !specialSymbols.contains(char) && char != '\'' && char != '"'
private def isIden(char: Char) = char.isLetter || char.isDigit || char == '_'
private def isSym(char: Char) = idenSymbols.contains(char)
private def isWhitespace(char: Char) = char.isWhitespace
private def nextString(terminator: Char) = nextEscapedChar.takeWhile(onLexerState(_ != terminator)).map(list => (list.map(_._1), (list.head._2 - 1) until list.lastOption.map(_._2).getOrElse(list.head._2)))
private def onLexerState(f: Char => Boolean)(lexerState: LexerState) = lexerState.chars.nonEmpty && f(lexerState.chars.head)

@targetName("nextChar")
private def next(f: Char => Boolean, c: (Char, FilePos)): State[LexerState, (String, FilePosRange)] = next(f, (c._1.toString, c._2.exlRange))

private def next(f: Char => Boolean, s: (String, FilePosRange)): State[LexerState, (String, FilePosRange)] = for {
  chars <- nextChar.takeWhile(onLexerState(f))
} yield (s._1 + chars.map(_._1).mkString, (s._2 - (s._1.length - 1)) until chars.lastOption.map(_._2.exlRange).getOrElse(s._2))

val tokenize: State[LexerState, List[LexerResult]] = nextElement.takeWhile(_._1.nonEmpty).map(_.filterNot(_.isIgnored))
