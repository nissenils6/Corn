package core

import scala.annotation.tailrec
import scala.compiletime.ops.int
import scala.language.implicitConversions
import scala.util.control.TailCalls.*

case class State[S, A](run: S => TailRec[(A, S)]) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => run(s).flatMap { (a, s) => f(a).run(s) })

  def map[B](f: A => B): State[S, B] = State(s => run(s).map { (a, s) => (f(a), s) })

  def apply(s: S): A = run(s).result._1
}

def repeat[S, A](state: State[S, A], count: Int): State[S, List[A]] = count match {
  case 0 => State(s => done(List(), s))
  case _ => for {
    a <- state
    as <- repeat(state, count - 1)
  } yield a :: as
}

@tailrec
def rep[S, A](state: State[S, A], count: Int, acc: State[S, List[A]]): State[S, List[A]] = count match {
  case 0 => acc
  case _ => rep(state, count - 1, for {
    a <- state
    as <- acc
  } yield a :: as)
}
