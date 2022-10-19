package fpInScala.chapter11

import fpInScala.chapter12.Exercises.Applicative
import fpInScala.chapter6.Exercises.State

/**
 * Functor is a type with `map` operation that satisfies laws:
 * - identity: `map(x)(a=>a)==x`
 * - composition: map(map(v)(g))(f) == map(v)(f compose g)
 *
 * Monad defines `unit` and `flatMap` operations. Monads are Functors because `map` can be implemented in terms of
 * `unit` and `flatMap`: `flatMap(a => unit(f(a)))`.
 * Monad laws:
 * - associative law: `x.flatMap(f).flatMap(g)==x.flatMap(a=>f(a).flatMap(g))`
 * - left identity law: `flatMap(x)(unit)==x`
 * - right identity law: `flatMap(unit(x))(f)==f(x)`
 *
 * Another minimal set of operations for Monad: `compose` and `unit` because `flatMap` can be implemented in terms of
 * `compose`. In this case laws are:
 * - associative law: `compose(compose(f,g),h)==compose(f,compose(g,h))`
 * - left identity law: `compose(f,unit)==f`
 * - right identity law: `compose(unit,f)==f`
 *
 * Third minimal set of operations for Monad: `map`, `unit`, `join`.
 *
 * Kleisli arrow - monadic function `a=>F[B]`, functions like this can be composed:
 * `compose(A=>F[B], B=>F[C]) == A=>F[C]`
 *
 * A monad is a monoid in the category of endofunctors.
 */
object Exercises {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }

  object Functor {
    val listFunctor = new Functor[List] {
      def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    }
  }

  // chapter 12 shows that Monad is applicative functor.
  trait Monad[M[_]] extends /*Functor[M]*/ Applicative[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    override def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => unit(f(a)))

    override def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    // Exercise 3
    override def sequence[A](lma: List[M[A]]): M[List[A]] = lma match {
      case Nil => unit(Nil)
      case h :: t => map2(h, sequence(t))((h2, t2) => h2 :: t2)
    }

    override def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = la match {
      case Nil => unit(Nil)
      case h :: t => map2(f(h), traverse(t)(f))((h2, t2) => h2 :: t2)
    }

    // Exercise 4
    override def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
      if (n <= 0) unit(Nil) else map2(ma, replicateM(n - 1, ma))(_ :: _)

    // Exercise 7
    def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

    // Exercise 8
    // Implement in terms of `compose`:
    def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)(())

    // Exercise 12
    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(x => x)

    // Exercise 13
    // Implement in terms of `join`:
    def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
  }

  object Monad {
    // Exercise 1
    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
    }

    val streamMonad = new Monad[Stream] {
      def unit[A](a: => A) = Stream(a)

      def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
    }

    val listMonad = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
    }

    // Exercise 2
    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }

    // Exercise 17
    val idMonad = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)

      override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
    }
  }

  // Exercise 17
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }
}
