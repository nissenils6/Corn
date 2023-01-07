package core

import core.Error.EXIT
import lex.Token
import sem.Datatype
import syn.Expr

import java.io.{FileReader, PrintWriter}
import scala.annotation.{tailrec, targetName, unused}
import scala.collection.mutable
import scala.io.BufferedSource
import scala.math
import scala.util.Using

extension[A] (list: List[Option[A]]) {
  def extract: Option[List[A]] = if list.forall(_.nonEmpty) then Some(list.map(_.get)) else None
}

extension[E, A] (list: List[Either[E, A]]) {
  def extract: Either[List[E], List[A]] = if list.forall(_.isRight) then Right(list.map(_.toOption.get)) else Left(list.collect { case Left(error) => error })
}

extension[E, A] (either: Either[E, A]) {
  def mapLeft[E2](f: E => E2): Either[E2, A] = either match {
    case Left(v) => Left(f(v))
    case _ => either.asInstanceOf[Either[E2, A]]
  }

  def mapBoth[E2, A2](f: E => E2, g: A => A2): Either[E2, A2] = either match {
    case Left(v) => Left(f(v))
    case Right(v) => Right(g(v))
  }
}

extension (int: Int) {
  def roundUp(powerOf2: Int): Int = (int & ~(powerOf2 - 1)) + (if (int & (powerOf2 - 1)) > 0 then powerOf2 else 0)
  def roundDown(powerOf2: Int): Int = if powerOf2 > 0 then math.floorDiv(int, powerOf2) * powerOf2 else int
}

case class ErrorComponent(range: FilePosRange, message: Option[String] = None) {
  override def toString: String = range.underline('-', message)
}

abstract class CompilerError extends Exception {
  @targetName("combine")
  def |(that: CompilerError): CompilerError = (this, that) match {
    case (error1: Error, error2: Error) => ErrorGroup(List(error1, error2))
    case (error: Error, group: ErrorGroup) => ErrorGroup(error :: group.errors)
    case (group: ErrorGroup, error: Error) => ErrorGroup(error :: group.errors)
    case (group1: ErrorGroup, group2: ErrorGroup) => ErrorGroup(group1.errors ::: group2.errors)
  }
}

case class Error(errorType: String, file: File, components: List[ErrorComponent], message: Option[String] = None) extends CompilerError {
  override def toString: String = s"$errorType error in '${file.name}':${message.map(message => s" $message:").getOrElse("")}\n\n${components.mkString("\n\n")}\n\n"
}

case class ErrorGroup(errors: List[Error]) extends CompilerError {
  override def toString: String = errors.mkString("\n")
}

object CompilerError {
  def unapply(compilerError: CompilerError): Option[String] = compilerError match {
    case error: Error if error.errorType == Error.EXIT => None
    case compilerError: CompilerError => Some(compilerError.toString)
  }
}

object Error {
  val CMD_LINE = "Command Line"
  val LEXICAL = "Lexical"
  val SYNTAX = "Syntax"
  val SEMANTIC = "Semantic"
  val EXIT = "Exit"

  def cmdLine(message: String, range: FilePosRange): Error =
    Error(CMD_LINE, range.file, List(ErrorComponent(range, Some(message))))

  def syntax(message: String, range: FilePosRange): Error =
    Error(SYNTAX, range.file, List(ErrorComponent(range, Some(message))))

  def semantic(message: String, range: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(ErrorComponent(range, Some(message))))
  
  def exit(cmdLine: File): Error =
    Error(EXIT, cmdLine, List.empty, None)
}

class File(val name: String, val source: String) {
  val newlines: Array[Int] = (for {
    (char, index) <- source.zipWithIndex
    if char == '\n'
  } yield index).toArray match {
    case array => Array(-1).concat(array).concat(Array(source.length))
  }

  val range: FilePosRange = FilePosRange(0, source.length, this)
  val lastRange: FilePosRange = range.after
}

object File {
  def apply(name: String, source: String): File = new File(name, source)

  def apply(filePath: String): Either[Throwable, File] = for {
    source <- slurpFile(filePath + ".txt")
  } yield new File(filePath.split('/').last, source)
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

def slurpFile(filePath: String): Either[Throwable, String] = Using(io.Source.fromFile(filePath))(_.mkString).toEither
def printFile(filePath: String, content: String): Unit = Using(new PrintWriter(filePath))(_.write(content))
