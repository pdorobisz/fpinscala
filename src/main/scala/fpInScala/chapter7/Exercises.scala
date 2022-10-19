package fpInScala.chapter7

import java.util.concurrent._
import java.util.concurrent.ExecutorService

object Exercises extends App {

  object Par {
    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

    // Exercise 7.5
    def sequence[A](list: List[Par[A]]): Par[List[A]] =
      list.foldLeft(unit[List[A]](Nil)) { (results, current) =>
        map2(results, current)(_ :+ _)
      }

    // Exercise 7.6
    def parFilter[A](list: List[A])(f: A => Boolean): Par[List[A]] = {
      val filtered = list.map(asyncF(x => if (f(x)) List(x) else List[A]()))
      map(sequence(filtered))(_.flatten)
    }

    // Exercise 7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => {
        val i = run(es)(n).get()
        run(es)(choices(i))
      }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(c => if (c) 0 else 1))(List(t, f))

    // Exercise 7.12
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => {
        val k = run(es)(key).get()
        run(es)(choices(k))
      }

    // Exercise 7.13
    def chooser[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val k = run(es)(p).get()
        run(es)(choices(k))
      }

    def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(c => if (c) t else f)

    def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices)

    // Exercise 7.14
    def join[A](a: Par[Par[A]]): Par[A] =
      es => {
        val p = run(es)(a).get()
        run(es)(p)
      }

    def flatMapViaJoin[A, B](p: Par[A])(choices: A => Par[B]): Par[B] = join(map(p)(choices))

    def flatMap[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val k = run(es)(p).get()
        run(es)(choices(k))
      }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

  }

}
