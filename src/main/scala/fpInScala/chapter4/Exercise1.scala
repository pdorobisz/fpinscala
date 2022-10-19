package fpInScala.chapter4

object Exercise1 {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
      flatMap(x => if (f(x)) Some(x) else None)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def main(args: Array[String]) {
    val f = (x: Int) => "**" + x + "**"
    val f2 = (x: Int) => if (x > 0) Some("^^" + x + "^^") else None

    println("Exercise 1: map: " + Some(123).map(f))
    println("Exercise 1: map: " + None.map(f))
    println("Exercise 1: getOrElse: " + Some(123).getOrElse("abc"))
    println("Exercise 1: getOrElse: " + None.getOrElse("abc"))
    println("Exercise 1: flatMap: " + Some(123).flatMap(f2))
    println("Exercise 1: flatMap: " + Some(0).flatMap(f2))
    println("Exercise 1: orElse: " + Some(123).orElse(Some(456)))
    println("Exercise 1: orElse: " + None.orElse(Some(456)))
    println("Exercise 1: filter: " + Some(1).filter(_ > 0))
    println("Exercise 1: filter: " + Some(0).filter(_ > 0))
  }
}
