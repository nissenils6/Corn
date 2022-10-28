import scala.annotation.{tailrec, targetName, unused}
import scala.collection.mutable

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

case class Error(errorType: String, message: String, range: FilePosRange) extends Exception {
  override def toString =
    s"$errorType error in '${range.file.name}' at $range: $message:\n${range.underline}\n"
}

object Error {
  val LEXICAL = "Lexical"
  val SYNTAX = "Syntax"
  val SEMANTIC = "Semantic"
  val INTERNAL = "Internal"

  def lexical(message: String, range: FilePosRange): Error =
    Error(LEXICAL, message, range)

  def unexpected(token: Token, expected: String): Error =
    Error(SYNTAX, s"Unexpected token '$token', expected $expected", token.range)

  def unexpected(expected: String, file: File): Error =
    Error(SYNTAX, s"Unexpected end of file, expected $expected", file.lastRange)

  def duplicate(name: String, range: FilePosRange): Error =
    Error(SEMANTIC, s"Duplicate element '$name'", range)

  def internal(message: String, file: File): Error =
    Error(INTERNAL, message, file.lastRange)

  def unimplemented(file: File): Error =
    Error(INTERNAL, s"Error message not implemented", file.lastRange)
}

case class File(name: String, source: String) {
  val newlines: Array[Int] = (for {
    (char, index) <- source.zipWithIndex
    if char == '\n'
  } yield index).toArray

  def row(pos: Int): Int = newlines.indexWhere(_ > pos)

  def col(pos: Int): Int = pos - newlines.find(_ > pos).getOrElse(0)

  def last: FilePos = FilePos(source.length, this)

  def lastRange: FilePosRange = FilePosRange(source.length, source.length + 1, this)
}

case class FilePos(pos: Int, file: File) {
  def +(int: Int): FilePos = FilePos(pos + int, file)

  def -(int: Int): FilePos = FilePos(pos - int, file)

  def until(filePos: FilePos): FilePosRange = FilePosRange(pos, filePos.pos + 1, file)

  def until(range: FilePosRange): FilePosRange = FilePosRange(pos, range.end + 1, file)

  def to(filePos: FilePos): FilePosRange = FilePosRange(pos, filePos.pos, file)

  def to(range: FilePosRange): FilePosRange = FilePosRange(pos, range.end, file)

  def range: FilePosRange = this until this

  def exlRange: FilePosRange = this to this
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
    println(file.newlines.mkString(", "))
    println(s"$start $end")
    val startIndex = file.newlines.indexWhere(_ < start)
    val endIndex = file.newlines.indexWhere(_ > end)
    println(s"$startIndex $endIndex")
    val builder = mutable.StringBuilder()
    for (i <- Range(startIndex, endIndex + 1)) {
      val lower = file.newlines(i).max(start)
      val upper = (if i <= file.newlines.length then file.newlines(i + 1) else file.source.length).min(end)
      builder.append(s"${file.source.substring(lower, upper)}").append(" " * lower + "^" * (upper - lower))
    }
    builder.toString()
  }
}
