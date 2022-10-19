package fpInScala.chapter2

/**
 * Implement currying.
 */
object Exercise3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  /////////////////////

  def main(args: Array[String]) {
    val add = (x: Int, y: Int) => x + y

    val add1 = curry(add)
    println("add1(3)(5): " + add1(3)(5))
  }
}
