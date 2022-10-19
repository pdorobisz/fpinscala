package fpInScala.chapter10

import fpInScala.chapter3.ExercisesTrees.{Branch, Leaf, Tree}

/**
 * Monoid is algebra - type together with monoid operations and set of laws. It consists of:
 * - type A
 * - `op` - associative binary operation: `op(op(x,y),z)==op(x,op(y,z))`, for any x,y,z of type A
 * - `zero` - identity value for `op` operation: `op(x,zero)==op(zero,x)==x`, for any x:A
 */
object Exercises {

  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  object Monoid {

    // Exercise 1
    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

      override def zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

      override def zero: Boolean = true
    }

    // Exercise 2
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

      override def zero: Option[A] = None
    }

    // Exercise 3
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

      override def zero: A => A = a => a
    }

    def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
      override def op(a1: A, a2: A): A = m.op(a2, a1)

      override def zero: A = m.zero
    }

    // Exercise 5
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldLeft(m.zero)((x, y) => m.op(x, f(y)))

    // Exercise 6
    def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

    def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldMap(as, dual(endoMonoid[B]))(a => b => f(a, b))(z)

    // Exercise 7
    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (v.size > 1) {
        val (v1, v2) = v.splitAt(v.size / 2)
        val r1 = foldMapV(v1, m)(f)
        val r2 = foldMapV(v2, m)(f)
        m.op(r1, r2)
      } else if (v.length == 1) {
        f(v.head)
      } else {
        m.zero
      }
    }

    // Exercise 9
    def isSorted(as: IndexedSeq[Int]): Boolean = {
      val m = new Monoid[(Int, Int, Boolean, Boolean)] {
        override def op(a1: (Int, Int, Boolean, Boolean), a2: (Int, Int, Boolean, Boolean)): (Int, Int, Boolean, Boolean) = (a1, a2) match {
          case ((min1, max1, asc1, desc1), (min2, max2, asc2, desc2)) =>
            val min = Math.min(min1, min2)
            val max = Math.max(max1, max2)
            val asc = max1 <= max2 && asc1 && asc2
            val desc = min1 >= min2 && desc1 && desc2
            (min, max, asc, desc)
        }

        override def zero: (Int, Int, Boolean, Boolean) = (Int.MinValue, Int.MaxValue, true, true)
      }
      val (_, _, asc, desc) = foldMapV(as, m)(x => (x, x, true, true))
      asc || desc
    }

    // Exercise 10
    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      override def op(a1: WC, a2: WC): WC = (a1, a2) match {
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        case (Stub(c), Part(lStub, words, rStub)) => Part(c + lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(c)) => Part(lStub, words, rStub + c)
        case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
          val w = if (rStub1.isEmpty && lStub2.isEmpty) 0 else 1
          Part(lStub1, words1 + words2 + w, rStub2)
      }

      override def zero: WC = Stub("")
    }

    // Exercise 15
    def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }

    // Exercise 17
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      override def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))

      override def zero: A => B = _ => B.zero
    }

    def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
      new Monoid[Map[K, V]] {
        def zero = Map[K, V]()

        def op(a: Map[K, V], b: Map[K, V]) =
          (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
            acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
          }
      }

    // Exercise 18
    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 11
  def wordCount(s: String): Int = {
    import Monoid._

    def word(s: String): Int = if (s.isEmpty) 0 else 1

    val result = foldMapV(s.toIndexedSeq, wcMonoid)(s => if (s.isWhitespace) Part("", 0, "") else Stub(s.toString))
    result match {
      case Stub(c) => word(c)
      case Part(lStub, words, rStub) => word(lStub) + word(rStub) + words
    }
  }

  trait Foldable[F[_]] {

    import Monoid._

    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid)(z)

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    // Exercise 15
    def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)
  }

  object Foldable {

    // Exercise 12
    object ListFoldable extends Foldable[List] {
      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    }

    object IndexedSeqFoldable extends Foldable[IndexedSeq] {
      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = Monoid.foldMapV(as, mb)(f)
    }

    object StreamFoldable extends Foldable[Stream] {
      override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    }

    // Exercise 13
    object TreeFoldable extends Foldable[Tree] {
      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
        case Leaf(value) => f(value, z)
        case Branch(left, right) =>
          val foldedRight = foldRight(right)(z)(f)
          foldRight(left)(foldedRight)(f)
      }

      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
        case Leaf(value) => f(z, value)
        case Branch(left, right) =>
          val foldedLeft = foldLeft(left)(z)(f)
          foldLeft(right)(foldedLeft)(f)
      }

      override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
        case Leaf(value) => f(value)
        case Branch(left, right) =>
          mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }
    }

    // Exercise 14
    object OptionFoldable extends Foldable[Option] {
      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    }

  }

  def main(args: Array[String]) {
    import Monoid._

    println("Exercise 6: " + foldLeftViaFoldMap(List("a", "b", "c"))("_")((a, b) => s"[$a,$b]"))
    println("Exercise 6: " + foldRightViaFoldMap(List("a", "b", "c"))("_")((a, b) => s"[$a,$b]"))

    println("Exercise 7: " + foldMapV(IndexedSeq("1", "2", "3", "4"), intAddition)(_.toInt))

    println("Exercise 9: " + isSorted(IndexedSeq(1, 2, 3, 4)))
    println("Exercise 9: " + isSorted(IndexedSeq(4, 3, 2, 1)))
    println("Exercise 9: " + isSorted(IndexedSeq(1, 4, 3, 2)))
    println("Exercise 9: " + isSorted(IndexedSeq(1)))
    println("Exercise 9: " + isSorted(IndexedSeq()))

    println("Exercise 11: " + wordCount("lorem"))
    println("Exercise 11: " + wordCount("lorem ipsum"))
    println("Exercise 11: " + wordCount("lorem ipsum dolor"))
    println("Exercise 11: " + wordCount("lorem ipsum dolor sit"))
  }
}
