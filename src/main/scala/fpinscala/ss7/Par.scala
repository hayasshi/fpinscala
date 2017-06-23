package fpinscala.ss7

import java.util.concurrent.{ Callable, TimeUnit, ExecutorService, Future }

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(), bf.get()))
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => a(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      ints.foldLeft()
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

}
