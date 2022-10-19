package fpInScala.chapter2

/**
 * Implement higher-order function that composes two functions.
 */
object Exercise5 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    x => f(g(x))
  }

  /////////////////////

  def main(args: Array[String]) {
    val f = (x: Int) => x + 2
    val g = (x: Int) => x * 2
    val h = compose(f, g)

    println("f(g(5)): " + h(5))
  }
}
