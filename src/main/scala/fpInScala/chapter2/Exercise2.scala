package fpInScala.chapter2

import scala.annotation.tailrec

/**
 * Implement isSorted.
 */
object Exercise2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

   @tailrec
    def go(previous: A, elements: Array[A]): Boolean = {
      elements.headOption match {
        case Some(current) => if (ordered(previous, current)) go(current, elements.tail) else false
        case None => true
      }
    }
    if (as.length == 0) true else go(as.head, as.tail)
  }

  /////////////////////

  def main(args: Array[String]) {
    val gt = (x: Int, y: Int) => x > y
    val lt = (x: Int, y: Int) => x < y

    val array1 = Array(1, 2, 3, 4, 5)
    val array2 = Array(1, 1, 2, 2, 2, 3)
    val array3 = Array(5, 4, 3, 2, 1)

    println("gt:")
    printResult(array1, gt)
    printResult(array2, gt)
    printResult(array3, gt)

    println("lt:")
    printResult(array1, lt)
    printResult(array2, lt)
    printResult(array3, lt)


  }

  def printResult[A](array: Array[A], gt: (A, A) => Boolean) {
    println(array.mkString("{", ",", "}") + ": " + isSorted(array, gt))
  }
}
