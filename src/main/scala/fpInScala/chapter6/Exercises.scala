package fpInScala.chapter6

import scala.annotation.tailrec

object Exercises {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Exercise 1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    val int2 = if (int == Int.MinValue) 0 else Math.abs(int)
    (int2, rng2)
  }

  // Exercise 2
  def double(rng: RNG): (Double, RNG) = {
    val (int, rng2) = nonNegativeInt(rng)
    val d = int / (Int.MaxValue.toDouble + 1)
    (d, rng2)
  }

  // Exercise 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // Exercise 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int, r: RNG, list: List[Int]): (List[Int], RNG) = {
      if (c > 0) {
        val (i, r2) = r.nextInt
        go(c - 1, r2, i :: list)
      } else {
        (list, r)
      }
    }
    go(count, rng, List())
  }

  ////////////////////////////////

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  ////////////////////////////////

  // Exercise 5
  def doubleViaMap(): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  // Exercise 7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def go(rng: RNG, fs: List[Rand[A]], list: List[A]): (List[A], RNG) = fs match {
      case h :: t =>
        val (a, r) = h(rng)
        go(r, t, a :: list)
      case _ => (list.reverse, rng)
    }
    rng => go(rng, fs, List())
  }

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // Exercise 8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // Exercise 9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))


  // Exercise 10
  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s2) = run(s)
        (f(a), s2)
      })

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      State(s => {
        val (a, s1) = run(s)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }

  object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] = State((a, _))

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List()))((s, acc) => s.map2(acc)(_ :: _))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }


  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(System.currentTimeMillis())

    println("Exercise 1: " + nonNegativeInt(rng))

    println("Exercise 2: " + double(rng))

    println("Exercise 3: " + intDouble(rng))
    println("Exercise 3: " + doubleInt(rng))
    println("Exercise 3: " + double3(rng))

    println("Exercise 4: " + ints(3)(rng))

    println("Exercise 5: " + doubleViaMap()(rng))

    println("Exercise 6: " + map2(int, double)((i, d) => s"$i, $d")(rng))

    println("Exercise 7: " + intsViaSequence(3)(rng))

    println("Exercise 8: " + nonNegativeLessThan(5)(rng))

    println("Exercise 9: " + mapViaFlatMap(int)(_ % 10)(rng))
    println("Exercise 9: " + map2ViaFlatMap(int, double)((i, d) => s"$i, $d")(rng))
  }
}
