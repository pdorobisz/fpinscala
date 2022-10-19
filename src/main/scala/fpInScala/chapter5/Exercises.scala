package fpInScala.chapter5

import fpInScala.chapter5.Exercises.Stream.{unfold, cons, empty}

import scala.annotation.tailrec

object Exercises {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    // Exercise 1
    def toList: List[A] = {
      @tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
      go(this, List()).reverse
    }

    // Exercise 2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    // Exercise 3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }

    // Exercise 4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // Exercise 5
    def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else Empty)

    // Exercise 6
    def headOptionViaFoldRight: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    // Exercise 7
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h).append(t))

    // Exercise 13
    def mapViaUnfold[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some(f(h()), t())
        case Empty => None
      }

    def takeViaUnfold(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(h, t), nn) if nn > 0 => Some(h(), (t(), nn - 1))
        case _ => None
      }

    def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if f(h()) => Some(h(), t())
        case _ => None
      }

    def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }

    def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
        case _ => None
      }

    // Exercise 14
    def startsWith[B](s: Stream[B]): Boolean =
      zipWithViaUnfold(s)((_, _)).forAll { case ((h1, h2)) => h1 == h2 }

    // Exercise 15
    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Empty => None
        case s: Cons[A] => Some((s, s.t()))
      } append Empty

    // Exercise 16
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, acc) => {
        lazy val (value, s) = acc
        val b2 = f(a, value)
        (b2, cons(b2, s))
      })._2
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

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // Exercise 8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // Exercise 9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // Exercise 10
    val fibs = {
      def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
      go(0, 1)
    }

    // Exercise 11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) map { case (v, s) => cons(v, unfold(s)(f)) } getOrElse Empty

    // Exercise 12
    val fibsViaUnfold =
      unfold((0, 1)) { case ((a, b)) => Some(a, (b, a + b)) }

    def fromViaUnfold(n: Int): Stream[Int] =
      unfold(n)(n => Some(n, n + 1))

    def constantViaUnfold[A](a: A): Stream[A] =
      unfold(a)(_ => Some(a, a))

    val onesViaUnfold: Stream[Int] =
      unfold(1)(_ => Some(1, 1))
  }

  def main(args: Array[String]) {
    println("Exercise 1: " + Stream(1, 2, 3, 4, 5, 6).toList)
    println("Exercise 1: " + Stream().toList)

    println("Exercise 2: " + Stream(1, 2, 3, 4, 5, 6).take(3).toList)
    println("Exercise 2: " + Stream(1, 2).take(3).toList)
    println("Exercise 2: " + Stream().take(2).toList)
    println("Exercise 2: " + Stream(1, 2, 3, 4, 5, 6).drop(3).toList)
    println("Exercise 2: " + Stream(1, 2).drop(3).toList)
    println("Exercise 2: " + Stream().drop(2).toList)

    println("Exercise 3: " + Stream(2, 4, 6, 1, 2, 3).takeWhile(_ % 2 == 0).toList)

    println("Exercise 4: " + Stream(2, 4, 6, 1, 2, 3).forAll(_ > 3))
    println("Exercise 4: " + Stream(2, 4, 6, 1, 2, 3).forAll(_ > 0))

    println("Exercise 5: " + Stream(2, 4, 6, 1, 2, 3).takeWhileViaFoldRight(_ > 1).toList)

    println("Exercise 6: " + Stream(1, 2).headOptionViaFoldRight)
    println("Exercise 6: " + Stream(1).headOptionViaFoldRight)
    println("Exercise 6: " + Stream().headOptionViaFoldRight)

    println("Exercise 7: map: " + Stream(2, 4, 6, 1, 2, 3).map(_ * 10).toList)
    println("Exercise 7: filter: " + Stream(2, 4, 6, 1, 2, 3).filter(_ % 2 == 0).toList)
    println("Exercise 7: append: " + Stream(2, 4, 6, 1, 2, 3).append(Stream(11, 12)).toList)
    println("Exercise 7: flatMap: " + Stream(2, 4, 6, 1, 2, 3).flatMap(x => Stream(2 * x)).toList)

    println("Exercise 8: " + Stream.constant(3).take(5).toList)

    println("Exercise 9: " + Stream.from(3).take(5).toList)

    println("Exercise 10: " + Stream.fibs.take(15).toList)

    println("Exercise 11: " + unfold(2)(x => Some((x, x * 2))).take(10).toList)

    println("Exercise 12: fibsViaUnfold: " + Stream.fibsViaUnfold.take(15).toList)
    println("Exercise 12: fromViaUnfold: " + Stream.fromViaUnfold(7).take(5).toList)
    println("Exercise 12: constantViaUnfold: " + Stream.constantViaUnfold(3).take(5).toList)
    println("Exercise 12: onesViaUnfold: " + Stream.onesViaUnfold.take(5).toList)

    println("Exercise 13: mapViaUnfold: " + Stream(2, 4, 6, 1, 2, 3).mapViaUnfold(_ * 10).toList)
    println("Exercise 13: takeViaUnfold:  " + Stream(1, 2, 3, 4, 5, 6).takeViaUnfold(3).toList)
    println("Exercise 13: takeWhileViaUnfold " + Stream(2, 4, 6, 1, 2, 3).takeWhileViaUnfold(_ > 1).toList)
    println("Exercise 13: zipWithViaUnfold" + Stream(2, 4, 6, 1, 2, 3).zipWithViaUnfold(Stream(20, 40, 60, 10, 20, 30))(_ + _).toList)
    println("Exercise 13: zipAllViaUnfold" + Stream(2, 4, 6, 1, 2, 3).zipAllViaUnfold(Stream(20, 40, 60, 10)).toList)

    println("Exercise 14: " + Stream(2, 4, 6, 1, 2, 3).startsWith(Stream(2, 4, 6)))
    println("Exercise 14: " + Stream(2, 4, 6, 1, 2, 3).startsWith(Stream(2, 4, 6, 5)))

    println("Exercise 15: " + Stream(1, 2, 3).tails.map(_.toList).toList)

    println("Exercise 16: " + Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}
