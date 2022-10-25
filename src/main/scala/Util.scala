import scala.annotation.{targetName, unused}
import scala.collection.mutable

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

  def underline: String = s"${file.source.substring(
    file.newlines.find(_ > start).getOrElse(0),
    file.newlines.find(_ > end).getOrElse(file.source.length)
  )}\n${" " * file.col(start) + "^" * (end - start)}"
}
