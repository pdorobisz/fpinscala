package fpInScala.chapter3

import scala.annotation.tailrec


object Exercises {

  sealed trait List[+A]

  case object Nil extends List[Nothing] {
    override def toString() = ""
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def toString() = head + ", " + tail.toString
  }

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    // Exercise 2
    def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, xs2) => xs2
    }

    // Exercise 3
    def setHead[A](xs: List[A], x: A): List[A] = Cons(x, List.tail(xs))

    // Exercise 4
    def drop[A](list: List[A], n: Int): List[A] = list match {
      case Nil => Nil
      case Cons(_, list2) => if (n > 0) drop(list2, n - 1) else list
    }

    // Exercise 5
    def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(x, list2) => if (f(x)) dropWhile(list2)(f) else list
    }

    // Exercise 6
    def init[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, list2) => Cons(x, init(list2))
    }

    // Exercise 9
    def length[A](list: List[A]): Int = foldRight(list, 0)((_, x: Int) => 1 + x)

    // Exercise 10
    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // Exercise 11
    def sum2(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def product2(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def length2[A](list: List[A]): Int = foldLeft(list, 0)((x, _) => 1 + x)

    // Exercise 12
    def reverse[A](list: List[A]): List[A] = foldLeft(list, List[A]())((acc, h) => Cons(h, acc))

    // Exercise 13
    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      foldRight(reverse(l), z)((a, b) => f(b, a))

    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(l), z)((a, b) => f(b, a))

    // Exercise 14
    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)(Cons(_, _))

    // Exercise 15
    def concat[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil: List[A])(append)

    // Exercise 16
    def add1(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    // Exercise 17
    def doubleToString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

    // Exercise 18
    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

    // Exercise 19
    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    // Exercise 20
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      concat(map(l)(f))

    // Exercise 21
    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l)(a => if (f(a)) List(a) else Nil)

    // Exercise 22
    def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) => l2
      case (_, Nil) => l1
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

    // Exercise 23
    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    // Exercise 24
    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
      case Nil => false
      case Cons(h, t) => if (startsWith(l, sub)) true else hasSubsequence(t, sub)
    }

    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) startsWith(t1, t2) else false
    }


    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  /////////////////////

  def main(args: Array[String]) {
    val list: List[Int] = List(1, 2, 3, 4, 5)

    // Exercise 1
    val x = list match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this one will match
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println("Exercise 1: " + x)

    // Exercise 2
    println("Exercise 2: " + List.tail(list))

    // Exercise 3
    println("Exercise 3: " + List.setHead(list, 6))

    // Exercise 4
    println("Exercise 4: " + List.drop(list, 3))
    println("Exercise 4: " + List.drop(list, 6))

    // Exercise 5
    println("Exercise 5: " + List.dropWhile(list)(_ < 3))

    // Exercise 6
    println("Exercise 6: " + List.init(list))

    // Exercise 9
    println("Exercise 9: " + List.length(list))
    println("Exercise 9: " + List.length(List(1)))
    println("Exercise 9: " + List.length(Nil))

    // Exercise 11
    println("Exercise 11: " + List.sum2(list))
    println("Exercise 11: " + List.product2(List(1.0, 2.0, 3.0)))
    println("Exercise 11: " + List.length(list))

    // Exercise 12
    println("Exercise 12: " + List.reverse(list))

    // Exercise 13
    val f: (Int, Int) => Int = (x, y) => {
      val s = x + y
      print(x + "+" + y + "=" + s + ", ")
      s
    }
    print("Exercise 13: ")
    List.foldRightViaFoldLeft(list, 0)(f)
    println()
    print("Exercise 13: ")
    List.foldLeftViaFoldRight(list, 0)(f)
    println()

    // Exercise 14
    println("Exercise 14: " + List.appendViaFoldRight(list, List(6, 7, 8)))

    // Exercise 15
    println("Exercise 15: " + List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

    // Exercise 16
    println("Exercise 16: " + List.add1(list))

    // Exercise 17
    println("Exercise 17: " + List.doubleToString(List(1.0, 2.0, 3.0, 4.0, 5.0)))

    // Exercise 18
    println("Exercise 18: " + List.map(list)(_ * 2))

    // Exercise 19
    println("Exercise 19: " + List.filter(list)(_ % 2 == 0))

    // Exercise 20
    println("Exercise 20: " + List.flatMap(list)((x) => List(x, x)))

    // Exercise 21
    println("Exercise 21: " + List.filterViaFlatMap(list)(_ % 2 == 1))

    // Exercise 22
    println("Exercise 22: " + List.addPairwise(List(1, 2, 3), List(10, 20, 30, 40, 50)))

    // Exercise 23
    println("Exercise 23: " + List.zipWith(List(1, 2, 3), List(10, 20, 30, 40, 50))(_ + _))

    // Exercise 24
    println("Exercise 24: " + List.hasSubsequence(list, List(3, 4)))
    println("Exercise 24: " + List.hasSubsequence(list, List(5,6)))
  }
}
