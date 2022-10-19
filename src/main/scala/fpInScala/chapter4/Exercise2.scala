package fpInScala.chapter4

object Exercise2 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => Math.pow(x - m, 2))))

  def main(args: Array[String]) {
    println("Exercise 2: " + variance(List(1, 2, 3, 4, 5)))
    println("Exercise 2: " + variance(List(1)))
    println("Exercise 2: " + variance(List()))
  }
}
