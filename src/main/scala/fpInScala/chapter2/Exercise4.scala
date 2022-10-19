package fpInScala.chapter2

/**
 * Implement uncurry.
 */
object Exercise4 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /////////////////////

  def main(args: Array[String]) {
    val add = (x: Int, y: Int) => x + y

    val curried = Exercise3.curry(add)
    val uncurried = uncurry(curried)

    println("uncurried(3, 5): " + uncurried(3, 5))
  }
}
