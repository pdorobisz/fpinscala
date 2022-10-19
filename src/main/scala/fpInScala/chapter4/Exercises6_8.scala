package fpInScala.chapter4

object Exercises6_8 {

  sealed trait Either[+E, +A] {

    // Exercise 6
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(value) => Left(value)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => f(value)
      case Left(value) => Left(value)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => Right(value)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        x <- this
        y <- b
      } yield f(x, y)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  // Exercise 7
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = a match {
    case Nil => Right(Nil)
    case h :: t => h flatMap (hh => sequence(t).map(hh :: _))
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = a match {
    case Nil => Right(Nil)
    case h :: t => f(h) flatMap (hh => traverse(t)(f).map(hh :: _))
  }

  def sequenceViaTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    traverse(a)(x => x)

  /////////////

  def main(args: Array[String]) {
    println("Exercise 6: map: " + Right(6).map(_ * 10))
    println("Exercise 6: map: " + Left("some error").map((x: Int) => x * 10))

    println("Exercise 6: flatMap: " + Right(6).flatMap(x => Right("<" + x + ">")))
    println("Exercise 6: flatMap: " + Left("some error").flatMap(x => Right("<" + x + ">")))

    println("Exercise 6: orElse: " + Right(6).orElse(Right(10)))
    println("Exercise 6: orElse: " + Left("some error").orElse(Right(10)))

    println("Exercise 6: map2: " + Right(6).map2(Right(10))(_ + _))
    println("Exercise 6: map2: " + Right(6).map2(Left("some error"))((x: Int, y: Int) => x + y))

    println("Exercise 7: sequence: " + sequence(List(Right(1), Right(2), Right(3), Right(4), Right(5))))
    println("Exercise 7: sequence: " + sequence(List(Right(1), Left(2), Right(3), Right(4), Right(5))))
    println("Exercise 7: sequenceViaTraverse: " + sequenceViaTraverse(List(Right(1), Right(2), Right(3), Right(4), Right(5))))
    println("Exercise 7: sequenceViaTraverse: " + sequenceViaTraverse(List(Right(1), Left(2), Right(3), Right(4), Right(5))))
    println("Exercise 7: traverse: " + traverse(List(1, 2, 3, 4, 5))(Right(_)))
    println("Exercise 7: traverse: " + traverse(List(1, 2, 3, 4, 5))(x => if (x % 2 == 0) Right(x) else Left("not even number: " + x)))
  }
}
