package core

import lex.Token
import sem.{Datatype, LocalVar}
import syn.Expr

import java.io.{FileReader, PrintWriter}
import scala.annotation.{tailrec, targetName, unused}
import scala.collection.mutable
import scala.io.BufferedSource
import scala.math
import scala.util.Using

extension[T] (list: List[Option[T]]) {
  def extract: Option[List[T]] = if list.forall(_.nonEmpty) then Some(list.map(_.get)) else None
}

extension (int: Int) {
  def roundUp(powerOf2: Int): Int = (int & ~(powerOf2 - 1)) + (if (int & (powerOf2 - 1)) > 0 then powerOf2 else 0)
  def roundDown(powerOf2: Int): Int = if powerOf2 > 0 then math.floorDiv(int, powerOf2) * powerOf2 else int
}

case class ErrorComponent(range: FilePosRange, message: Option[String] = None) {
  override def toString: String = range.underline('-', message)
}

abstract class CompilerError extends Exception {
  
}

case class Error(errorType: String, file: File, components: List[ErrorComponent], message: Option[String] = None) extends CompilerError {
  override def toString: String = s"$errorType error in '${file.name}':${message.map(message => s" $message:").getOrElse("")}\n\n${components.mkString("\n\n")}\n\n"
}

case class ErrorGroup(errors: List[Error]) extends CompilerError {
  override def toString: String = errors.mkString("\n")
}

object Error {
  val CMD_LINE = "Command Line"
  val LEXICAL = "Lexical"
  val SYNTAX = "Syntax"
  val SEMANTIC = "Semantic"
  val INTERNAL = "Internal"

  def cmdLine(message: String, range: FilePosRange): Error =
    Error(CMD_LINE, range.file, List(ErrorComponent(range, Some(message))))

  def lexical(message: String, range: FilePosRange): Error =
    Error(LEXICAL, range.file, List(ErrorComponent(range, Some(message))))

  def syntax(message: String, range: FilePosRange): Error =
    Error(SYNTAX, range.file, List(ErrorComponent(range, Some(message))))

  def semantic(message: String, range: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(ErrorComponent(range, Some(message))))

  def internal(file: File): Error =
    Error(INTERNAL, file, List.empty)

  def internal(range: FilePosRange): Error =
    Error(INTERNAL, range.file, List(ErrorComponent(range)))

  def internal(message: String, range: FilePosRange): Error =
    Error(INTERNAL, range.file, List(ErrorComponent(range, Some(message))))

  def unexpected(expr: Expr, expected: String): Error =
    Error(SYNTAX, expr.range.file, List(ErrorComponent(expr.range, Some(s"Unexpected expression, expected '$expected'"))))

  def unexpected(token: Token, expected: String): Error =
    Error(SYNTAX, token.range.file, List(ErrorComponent(token.range, Some(s"Unexpected token '$token', expected '$expected'"))))

  def unexpected(expected: String, file: File): Error =
    Error(SYNTAX, file, List(ErrorComponent(file.lastRange, Some(s"Unexpected end of file, expected '$expected'"))))

  def typeMismatch(datatype: Datatype, expectedDatatype: Datatype, range: FilePosRange, patternRange: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(
      ErrorComponent(patternRange, Some(s"'$expectedDatatype' expected")),
      ErrorComponent(range, Some(s"'$datatype' provided"))
    ), Some("Type mismatch"))

  def datatypeExpected(range: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(ErrorComponent(range, Some(s"Expected compile-time expression evaluating to value of type 'Type'"))))
}

class File(val name: String, val source: String) {
  val newlines: Array[Int] = (for {
    (char, index) <- source.zipWithIndex
    if char == '\n'
  } yield index).toArray match {
    case array => Array(-1).concat(array).concat(Array(source.length))
  }

  def lastRange: FilePosRange = FilePosRange(source.length, source.length + 1, this)
}

object File {
  def apply(name: String, source: String): File = new File(name, source)

  def apply(filePath: String): File = new File(filePath.split('/').last, slurpFile(filePath + ".txt"))
}

case class FilePosRange(start: Int, end: Int, file: File) {
  @targetName("union")
  def |(range: FilePosRange): FilePosRange = FilePosRange(start.min(range.start), end.max(range.end), file)

  def after: FilePosRange = FilePosRange(end, end + 1, file)

  override def toString: String = s"$start..$end"

  def underline(underlineChar: Char, message: Option[String] = None): String = {
    val lineNumberSpace = 6

    def line(lineNumber: Int) = {
      val lineStart = file.newlines(lineNumber) + 1
      val lineEnd = file.newlines(lineNumber + 1)

      val startIndex = start.max(lineStart)
      val endIndex = end.min(lineEnd)

      val lineString = file.source.substring(lineStart, lineEnd).replace('\t', ' ')
      val underlineString = (" " * (startIndex - lineStart)) + (underlineChar.toString * (endIndex - startIndex))

      val fullLineString = s"${lineNumber + 1}:".padTo(lineNumberSpace, ' ') + "| " + lineString
      val fullUnderlineString = s"${" " * lineNumberSpace}| $underlineString"

      s"$fullLineString\n$fullUnderlineString"
    }

    val startLine = file.newlines.lastIndexWhere(_ < start)
    val endLine = file.newlines.indexWhere(_ >= end)

    val content = (startLine until endLine).map(line).mkString("\n")
    message match {
      case Some(message) if startLine + 1 == endLine => s"$content\n${" " * lineNumberSpace}| ${" " * (start - file.newlines(startLine) - 1)}$message"
      case Some(message) => s"$content\n${" " * lineNumberSpace}| $message"
      case None => content
    }
  }
}

def slurpFile(filePath: String): String = Using(io.Source.fromFile(filePath))(_.mkString).get
def printFile(filePath: String, content: String): Unit = Using(new PrintWriter(filePath))(_.write(content))
