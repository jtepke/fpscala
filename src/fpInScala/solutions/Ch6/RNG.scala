package fpInScala.solutions.Ch6

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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, newRng) = rng.nextInt
    if (i < 0)
      if (i == Int.MinValue) nonNegativeInt(newRng)
      else (-i, newRng)
    else (i, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, newRng) = nonNegativeInt(rng)
    (i / Int.MaxValue, newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, newRng1) = rng.nextInt
    val (d, newRng2) = double(newRng1)
    ((i, d), newRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), newRnd) = intDouble(rng)
    ((d, i), newRnd)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 1) {
      val (i, nRng) = rng.nextInt
      (List(i), nRng)
    } else {
      val (i1, nRng1) = rng.nextInt
      val (i2, nRng2) = ints(count - 1)(nRng1)
      (i1 :: i2, nRng2)
    }
  }
}

object test extends App {
  val sRng = SimpleRNG(1L)
  println(sRng.ints(10)(sRng))
}
