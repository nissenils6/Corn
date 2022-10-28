import scala.annotation.{tailrec, targetName, unused}
import scala.collection.mutable
import scala.math

case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (v, ns) = run(s)
    (f(v), ns)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (v, ns) = run(s)
    f(v).run(ns)
  })

  def takeWhile(condition: S => Boolean): State[S, List[A]] = State(s => {
    @tailrec def rec(l: List[A], s: S): (List[A], S) = {
      if (condition(s)) {
        val (v, ns) = run(s)
        rec(v :: l, ns)
      } else {
        (l, s)
      }
    }

    val (la, ns) = rec(List[A](), s)
    (la.reverse, ns)
  })

  def eval(s: S): A = run(s)._1
}

object State {
  def insert[S, A](a: A): State[S, A] = State(s => (a, s))
}

extension[T] (list: List[Option[T]])
  def extract: Option[List[T]] = if list.forall(_.nonEmpty) then Some(list.map(_.get)) else None

abstract class Error extends Exception {
  def errorType: String
  def range: FilePosRange
  def format: String
  override def toString = s"$errorType error in '${range.file.name}' at $range: $format"
}

case class SimpleError(errorType: String, message: String, range: FilePosRange) extends Error {
  override def format: String = s"$message:\n\n${range.underline}\n"
}

case class DuplicateError(elementType: String, name: String, range: FilePosRange, alreadyDefinedRange: FilePosRange) extends Error {
  override def errorType: String = Error.SEMANTIC

  override def format: String = s"Duplicate $elementType '$name':\n\n${range.underline}\n\n'$name' is already defined here:\n\n${alreadyDefinedRange.underline}\n"
}

case class AssignTypeMismatchError(datatype: Datatype, expectedDatatype: Datatype, range: FilePosRange, patternRange: FilePosRange) extends Error {
  override def errorType: String = Error.SEMANTIC

  override def format: String = s"Type mismatch in let statement, return type of the expression is '$datatype':\n\n${range.underline}\n\nThe pattern expected value of type '$expectedDatatype':\n\n${patternRange.underline}\n"
}

object Error {
  val LEXICAL = "Lexical"
  val SYNTAX = "Syntax"
  val SEMANTIC = "Semantic"
  val INTERNAL = "Internal"

  def lexical(message: String, range: FilePosRange): Error =
    SimpleError(LEXICAL, message, range)

  def unexpected(token: Token, expected: String): Error =
    SimpleError(SYNTAX, s"Unexpected token '$token', expected $expected", token.range)

  def unexpected(expected: String, file: File): Error =
    SimpleError(SYNTAX, s"Unexpected end of file, expected $expected", file.lastRange)

  def semantic(message: String, range: FilePosRange): Error =
    SimpleError(SEMANTIC, message, range)

  def duplicate(elementType: String, name: String, range: FilePosRange, alreadyDefinedRange: FilePosRange): Error =
    DuplicateError(elementType, name, range, alreadyDefinedRange)

  def assignTypeMismatch(datatype: Datatype, expectedDatatype: Datatype, range: FilePosRange, patternRange: FilePosRange): Error =
    AssignTypeMismatchError(datatype, expectedDatatype, range, patternRange)

  def datatypeExpected(found: Datatype, range: FilePosRange): Error =
    SimpleError(SEMANTIC, s"Expression expected to compile-time evaluate to 'Type', found '$found'", range)

  def datatypeExpected(range: FilePosRange): Error =
    SimpleError(SEMANTIC, s"Expression expected to compile-time evaluate to 'Type'", range)

  def internal(message: String, range: FilePosRange): Error =
    SimpleError(INTERNAL, message, range)

  def unimplemented(file: File): Error =
    SimpleError(INTERNAL, s"Error message not implemented", file.lastRange)
}

case class File(name: String, source: String) {
  val newlines: Array[Int] = (for {
    (char, index) <- source.zipWithIndex
    if char == '\n'
  } yield index).toArray

  def lastRange: FilePosRange = FilePosRange(source.length, source.length + 1, this)
}

case class FilePos(pos: Int, file: File) {
  def +(int: Int): FilePos = FilePos(pos + int, file)

  def -(int: Int): FilePos = FilePos(pos - int, file)
  def until(range: FilePosRange): FilePosRange = FilePosRange(pos, range.end + 1, file)
  def to(range: FilePosRange): FilePosRange = FilePosRange(pos, range.end, file)
  def range: FilePosRange = this until this
  def until(filePos: FilePos): FilePosRange = FilePosRange(pos, filePos.pos + 1, file)
  def exlRange: FilePosRange = this to this
  def to(filePos: FilePos): FilePosRange = FilePosRange(pos, filePos.pos, file)
}

case class FilePosRange(start: Int, end: Int, file: File) {
  def +(int: Int): FilePosRange = FilePosRange(start + int, end + int, file)

  def -(int: Int): FilePosRange = FilePosRange(start - int, end - int, file)

  def until(filePos: FilePos): FilePosRange = FilePosRange(start, filePos.pos + 1, file)

  def until(range: FilePosRange): FilePosRange = FilePosRange(start, range.end + 1, file)

  def to(filePos: FilePos): FilePosRange = FilePosRange(start, filePos.pos, file)

  def to(range: FilePosRange): FilePosRange = FilePosRange(start, range.end, file)

  override def toString: String = s"$start..$end"

  def underline: String = {
    var lineNum = file.newlines.lastIndexWhere(_ < start) + 1
    val lineNumSpace = math.log10((file.newlines.indexWhere(_ >= end) match {
      case -1 => file.newlines.length
      case num => num
    }) + 2).ceil.toInt + 2
    val lineStart = file.newlines.findLast(_ < start).getOrElse(0)
    val lineEnd = file.newlines.find(_ >= end).getOrElse(file.source.length)

    val (code, under) = file.source.substring(lineStart, lineEnd).zipWithIndex.flatMap {
      case ('\n', _) => ('\n', '\n') :: (s"${lineNum += 1; lineNum}:".padTo(lineNumSpace, ' ') + "| ").toList.zip((" " * lineNumSpace + "| ").toList)
      case ('\t', index) if start until end contains (lineStart + index) => List.fill(4)((' ', '^'))
      case (char, index) if start until end contains (lineStart + index) => List((char, '^'))
      case ('\t', _) => List.fill(4)((' ', ' '))
      case (char, _) => List((char, ' '))
    }.unzip

    code.mkString.split("\n+").zip(under.mkString.split("\n+")).filter(t => t._1.length + t._2.length > 0).map(t => s"${t._1}\n${t._2}").mkString("\n")
  }
}
