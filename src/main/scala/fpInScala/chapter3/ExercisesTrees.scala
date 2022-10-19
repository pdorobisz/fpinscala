package fpInScala.chapter3

object ExercisesTrees {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // Exercise 25
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(t1, t2) => 1 + size(t1) + size(t2)
    }

    // Exercise 26
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(value) => value
      case Branch(t1, t2) => maximum(t1) max maximum(t2)
    }

    // Exercise 27
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(t1, t2) => (depth(t1) max depth(t2)) + 1
    }

    // Exercise 28
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
    }

    // Exercise 29
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)(_ + _ + 1)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(x => x)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(_ => 1)((a, b) => (a max b) + 1)

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
  }

  def main(args: Array[String]) {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3))

    // Exercise 25
    println("Exercise 25: " + Tree.size(tree))

    // Exercise 26
    println("Exercise 26: " + Tree.maximum(tree))

    // Exercise 27
    println("Exercise 27: " + Tree.depth(tree))

    // Exercise 28
    println("Exercise 28: " + Tree.map(tree)(_ * 10))

    // Exercise 29
    println("Exercise 29: sizeViaFold: " + Tree.sizeViaFold(tree))
    println("Exercise 29: maximumViaFold: " + Tree.maximumViaFold(tree))
    println("Exercise 29: depthViaFold: " + Tree.depthViaFold(tree))
    println("Exercise 29: mapViaFold: " + Tree.mapViaFold(tree)(_ * 10))
  }
}
