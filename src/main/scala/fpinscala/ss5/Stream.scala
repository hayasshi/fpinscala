package fpinscala.ss5

import java.util.NoSuchElementException

trait Stream[+A] {
  import Stream._

  // I see the answer.
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def head: A = headOption.get

  def tail: Stream[A] = this match {
    case Empty => throw new NoSuchElementException("Stream is empty.")
    case Cons(_, t) => t()
  }

  def reverse: Stream[A] =
    foldRight(empty[A])((a, b) => cons(a, b))

  def toList: List[A] = {
    def reverseBuild(s: Stream[A], l: List[A]): List[A] = {
      s match {
        case Empty => l
        case Cons(h, t) => reverseBuild(t(), h() :: l)
      }
    }
    reverseBuild(this, List.empty[A]).reverse
  }

  def take(n: Int): Stream[A] = {
    def take0(nn: Int, from: Stream[A], to: Stream[A]): Stream[A] = {
      if (nn <= 0 || from == Empty) {
        to
      } else {
        take0(nn -1, from.tail, cons(from.head, to))
      }
    }
    take0(n, this, empty).reverse
  }

  def drop(n: Int): Stream[A] = {
    def drop0(nn: Int, to: Stream[A]): Stream[A] = {
      if (nn <= 0 || to == Empty) to
      else drop0(nn - 1, to.tail)
    }
    drop0(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

}