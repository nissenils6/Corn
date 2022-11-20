package core

import lex.Token
import syn.Expr
import sem.Datatype

import scala.annotation.{tailrec, targetName, unused}
import scala.collection.mutable
import scala.io.BufferedSource
import scala.math

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

case class Error(errorType: String, file: File, components: List[ErrorComponent], message: Option[String] = None) extends Exception {
  override def toString: String = s"$errorType error in '${file.name}':${message.map(message => s" $message:").getOrElse("")}\n\n${components.mkString("\n\n")}\n\n"
}

case class ErrorGroup(errors: List[Error]) extends Exception {
  override def toString: String = errors.mkString("\n")
}

object Error {
  val LEXICAL = "Lexical"
  val SYNTAX = "Syntax"
  val SEMANTIC = "Semantic"
  val INTERNAL = "Internal"

  def unexpected(expr: Expr, expected: String): Error =
    Error(SYNTAX, expr.range.file, List(ErrorComponent(expr.range, Some(s"Unexpected expression, expected '$expected'"))))

  def unexpected(token: Token, expected: String): Error =
    Error(SYNTAX, token.range.file, List(ErrorComponent(token.range, Some(s"Unexpected token '$token', expected '$expected'"))))

  def unexpected(expected: String, file: File): Error =
    Error(SYNTAX, file, List(ErrorComponent(file.lastRange, Some(s"Unexpected end of file, expected '$expected'"))))

  def semantic(message: String, range: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(ErrorComponent(range, Some(message))))

  def typeMismatch(datatype: Datatype, expectedDatatype: Datatype, range: FilePosRange, patternRange: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(
      ErrorComponent(patternRange, Some(s"'$expectedDatatype' expected")),
      ErrorComponent(range, Some(s"'$datatype' provided"))
    ), Some("Type mismatch"))

  def datatypeExpected(range: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(ErrorComponent(range, Some(s"Expected compile-time expression evaluating to value of type 'Type'"))))

  def todo(file: File): Error =
    Error(INTERNAL, file, List())

  def todo(range: FilePosRange): Error =
    Error(INTERNAL, range.file, List(ErrorComponent(range)))

  def todo(message: String, range: FilePosRange): Error =
    Error(INTERNAL, range.file, List(ErrorComponent(range, Some(message))))
}

case class File(filePath: String) {
  val name: String = filePath.split('/').last.split('.').head
  val source: String = {
    val fileContent = io.Source.fromFile(filePath)
    val source = fileContent.getLines().mkString("\n")
    fileContent.close()
    source
  }

  val newlines: Array[Int] = (for {
    (char, index) <- source.zipWithIndex
    if char == '\n'
  } yield index).toArray match {
    case array => Array(-1).concat(array).concat(Array(source.length))
  }

  def lastRange: FilePosRange = FilePosRange(source.length, source.length + 1, this)
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
