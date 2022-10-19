package fpInScala.chapter4

import java.util.regex._

object Exercises3_5 {

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  // Exercise 3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  // 'for comprehension' is expanded by compiler to following code:
    a flatMap (aa => b map (bb => f(aa, bb)))

  // Exercise 4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((h, t) => map2(h, t)(_ :: _))

  // Exercise 5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)


  ////////////

  def main(args: Array[String]) {
    println("Exercise 3: " + map2(Some(1), Some(10))(_ + " - " + _))
    println("Exercise 3: " + map2(None, Some(10))((x: Int, y: Int) => x + " - " + y))
    println("Exercise 3: " + map2_1(Some(1), Some(10))(_ + " - " + _))
    println("Exercise 3: " + map2_1(None, Some(10))((x: Int, y: Int) => x + " - " + y))

    println("Exercise 4: " + sequence(List(Some(1), Some(2), Some(3), Some(4), Some(5))))
    println("Exercise 4: " + sequence(List(Some(1), Some(2), None, Some(4), Some(5))))

    println("Exercise 5: " + traverse(List(1, 2, 3, 4, 5))(x => Some(x * 10)))
    println("Exercise 5: " + traverse(List(1, 2, 3, 4, 5))(x => if (x % 2 == 1) None else Some(x)))
    println("Exercise 5: " + sequenceViaTraverse(List(Some(1), Some(2), Some(3), Some(4), Some(5))))
    println("Exercise 5: " + sequenceViaTraverse(List(Some(1), Some(2), None, Some(4), Some(5))))
  }
}
