package fpInScala.chapter12

import fpInScala.chapter10.Exercises.{Foldable, Monoid}
import fpInScala.chapter11.Exercises.{Functor, Id, Monad}
import fpInScala.chapter6.Exercises.State
import fpInScala.chapter6.Exercises.State._

/**
 * Applicative is a Functor (provides map) where minimal set of operations can be:
 * - unit and map2
 * - unit and apply
 * - unit, map and product
 *
 * Applicative laws:
 * - left and right identity
 *   applicative should satisfy functor laws:
 *   identity: map(v)(id) == v
 *   composition: map(map(v)(g))(f) == map(v)(f compose g)
 *
 *   these laws imply left and right identity because map is implemented in terms of map2:
 *   left identity: map2(unit(()), fa)((_, a) => a ) == fa
 *   right identity: map2(fa, unit(()))((a, _) => a ) == fa
 *
 *   - associativity
 *   product(product(fa, fb), fc) ~ product(fa, product(fb, fc)) // types don't line up
 *   product(product(fa, fb), fc) == product(fa, product(fb, fc)).map { case (a, (b, c)) => ((a, b), c) } // using map just to line up types
 *
 *   - naturality
 *   map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))
 *   where: productF[I,O,I2,O2](f:I=>O,g:I2=>O2): (I,I2)=>(O,O2) = (i,i2) =>(f(i),g(i2))
 *
 * Monad is an Applicative (Applicative's operations can be implemented in terms of Monad's), requires minimal set of
 * operations that can be:
 * - unit and flatMap
 * - unit and compose
 * - unit, map and join
 *
 * Applicative vs Monad
 * - Monad is Applicative but not all applicative functors are monads
 * - In Applicative computations are fixed where in Monad current computation depends on previous one. This gives
 * interpreter more information up front and possibly allows more efficient implementation strategy.
 * - Applicative compose, Monads don't
 * - Applicative is good for validating multiple values and to accumulate all errors in a single list. With Monad we
 * would only get first error.
 */
object Exercises {

  trait Applicative[F[_]] extends Functor[F] {

    def unit[A](a: => A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, list) => map2(f(a), list)(_ :: _))

    // Exercise 1
    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      if (n <= 0) unit(Nil) else map2(fa, replicateM(n - 1, fa))(_ :: _)

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

    // Exercise 2
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

    def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)

    //Exercise 3
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

    // Exercise 8
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this

      new Applicative[({type f[x] = (F[x], G[x])})#f] {

        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
          (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
    }

    // Exercise 9
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          self.map2(fa, fb)((g1, g2) => G.map2(g1, g2)(f))
      }
    }

    // Exercise 12
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      ofa.foldLeft(unit(Map.empty[K, V])) { case (fmap, (k, fv)) =>
        map2(fmap, fv)((m, v) => m + (k -> v))
      }
  }

  // Exercise 5
  def eitherMonad[E]: Monad[({type f[X] = Either[E, X]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma.flatMap(f)
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  object Applicative {
    type Const[A, B] = A

    implicit def monoidApplicative[M](M: Monoid[M]) =
      new Applicative[({type f[x] = Const[M, x]})#f] {
        def unit[A](a: => A): M = M.zero

        override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
      }

    val streamApplicative = new Applicative[Stream] {
      override def unit[A](a: => A): Stream[A] = Stream.continually(a)

      override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
        fa.zip(fb).map(f.tupled)
    }

    // Exercise 6
    def validationApplicative[E]: Applicative[({type f[X] = Validation[E, X]})#f] = new Applicative[({type f[X] = Validation[E, X]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => unit(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (f@Failure(_, _), _) => f
          case (_, f@Failure(_, _)) => f
        }
    }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
    import Applicative._

    def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
      traverse(fma)(ga => ga)

    override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
        as)(f)(monoidApplicative(mb))

    // Exercise 14
    def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => Id(f(a)))(Monad.idMonad).value

    def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

    def zipWithIndex_[A](ta: F[A]): F[(A,Int)] =
      traverseS(ta)((a: A) => (for {
        i <- get[Int]
        _ <- set(i + 1)
      } yield (a, i))).run(0)._1

    def toList_[A](fa: F[A]): List[A] =
      traverseS(fa)((a: A) => (for {
        as <- get[List[A]] // Get the current state, the accumulated list.
        _  <- set(a :: as) // Add the current element and set the new list as the new state.
      } yield ())).run(Nil)._2.reverse

    def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) => (for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _  <- set(s2)
      } yield b)).run(s)

    // Exercise 16
    def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
  }

  object Traverse {
    // Exercise 13
    val listTraverse = new Traverse[List] {
      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        fa.foldRight(G.unit(List.empty[B]))((element, list) => G.map2(f(element), list)(_ :: _))
    }

    val optionTraverse = new Traverse[Option] {
      override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = fa match {
        case Some(value) => G.map(f(value))(Some(_))
        case None => G.unit(None)
      }
    }

    val treeTraverse = new Traverse[Tree] {
      override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
        val h = f(fa.head)
        val t = listTraverse.traverse(fa.tail)(t => traverse(t)(f))
        G.map2(h, t)(Tree(_, _))
      }
    }
  }

}
