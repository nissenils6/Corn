import scala.annotation.tailrec

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

extension[A, B] (tuple: (A, B))
  def map[C, D](fa: A => C, fb: B => D) = {
    val (a, b) = tuple
    (fa(a), fb(b))
  }
  def mapLeft[C](fa: A => C): (C, B) = {
    val (a, b) = tuple
    (fa(a), b)
  }
  def mapRight[C](fb: B => C): (A, C) = {
    val (a, b) = tuple
    (a, fb(b))
  }

case class Error(errorType: String, message: String, range: FilePosRange) extends Exception {
  override def toString =
    s"$errorType error in '${range.file.name}' at $range: $message:\n${range.underline}\n"
}

object Error {
  val LEXICAL = "Lexical"
  val SYNTAX = "Syntax"
  val SEMANTIC = "Semantic"

  def lexical(message: String, range: FilePosRange): Error =
    Error(LEXICAL, message, range)

  def unexpected(token: Token, expected: String): Error =
    Error(SYNTAX, s"Unexpected token '$token', expected $expected", token.range)

  def unexpected(expected: String, file: File): Error =
    Error(SYNTAX, s"Unexpected end of file, expected $expected", file.lastRange)
    
  def duplicate(name: String, range: FilePosRange): Error =
    Error(SEMANTIC, s"Duplicate element '$name'", range)
    
  def unimplemented(file: File): Error =
    Error(SEMANTIC, s"Error message not implemented", file.lastRange)
}
