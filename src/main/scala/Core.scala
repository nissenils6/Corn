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

extension[T] (list: List[Option[T]]) {
  def extract: Option[List[T]] = if list.forall(_.nonEmpty) then Some(list.map(_.get)) else None
}

extension (int: Int) {
  def roundUp(powerOf2: Int): Int = (int & ~(powerOf2 - 1)) + (if (int & (powerOf2 - 1)) > 0 then powerOf2 else 0)
  def roundDown(powerOf2: Int): Int = if powerOf2 > 0 then math.floorDiv(int, powerOf2) * powerOf2 else int
}

extension[T, I <: IterableOnce[T]] (it: I) {
  def accumulate[E](startValue: E)(f: (T, E) => E): E = {
    var accumulator = startValue
    for (element <- it) {
      accumulator = f(element, accumulator)
    }
    accumulator
  }
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

  def datatypeExpected(found: Datatype, range: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(ErrorComponent(range, Some(s"Expression expected to compile-time evaluate to 'Type', found '$found'"))))

  def datatypeExpected(range: FilePosRange): Error =
    Error(SEMANTIC, range.file, List(ErrorComponent(range, Some(s"Expression expected to compile-time evaluate to 'Type'"))))

  def todo(file: File): Error =
    Error(INTERNAL, file, List())

  def todo(range: FilePosRange): Error =
    Error(INTERNAL, range.file, List(ErrorComponent(range)))

  def todo(message: String, range: FilePosRange): Error =
    Error(INTERNAL, range.file, List(ErrorComponent(range, Some(message))))
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

  def underline(underlineChar: Char, message: Option[String] = None): String = {
    val startLine = file.newlines.lastIndexWhere(_ < start)
    val endLine = file.newlines.indexWhere(_ >= end) match {
      case -1 => file.newlines.length
      case num => num
    }

    val startIndex = file.newlines.findLast(_ < start).getOrElse(0)
    val endIndex = file.newlines.find(_ >= end).getOrElse(file.source.length)

    var lineNum = startLine + 1
    val lineNumSpace = math.log10((endLine) + 2).ceil.toInt + 2

    val (code, under) = file.source.substring(startIndex, endIndex).zipWithIndex.flatMap {
      case ('\n', _) => ('\n', '\n') :: (s"${lineNum += 1; lineNum}:".padTo(lineNumSpace, ' ') + "| ").toList.zip((" " * lineNumSpace + "| ").toList)
      case ('\t', index) if start until end contains (startIndex + index) => List.fill(4)((' ', underlineChar))
      case (char, index) if start until end contains (startIndex + index) => List((char, underlineChar))
      case ('\t', _) => List.fill(4)((' ', ' '))
      case (char, _) => List((char, ' '))
    }.unzip

    val string = code.mkString.split("\n+").zip(under.mkString.split("\n+")).filter(t => t._1.length + t._2.length > 0).map(t => s"${t._1}\n${t._2}").mkString("\n")

    val indentation = " " * lineNumSpace + "| " + (if startLine == endLine - 1 then " " * (start - startIndex - 1 + file.source.substring(startIndex, start).count(_ == '\t') * 3) else "")
    s"$string${message.map(_.split('\n')).getOrElse(Array[String]()).map(msgLine => s"\n$indentation$msgLine").mkString}"
  }
}
