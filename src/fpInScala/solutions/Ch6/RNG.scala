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
    (i / Int.MaxValue.toDouble, newRng)
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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)


  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleMapImpl: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  //TODO think about types -.-
  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
  //    case List(rng1) => {
  //      val (v, rng2) = map(rng1)(x => x)
  //      (List(v):List[A], rng2:Rand[List[A]])
  //    }
  //    case rng1::rngs => {
  //      val (v, rng2) = map(rng1)(x => x)
  //      val (vs, rng3) = sequence(rng2)
  //      (v :: vs, rng3)
  //    }
  //  }
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //not as trivial as it looks like
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng1 => {
      val (v, rng2) = f(rng1)
      g(v)(rng2)
    }

  def mapFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

  def map2FlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(x => flatMap(rb)(y => unit(f(x, y))))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

case class State[S, +A](run: S => (A, S)) {
  type State[S, +A] = S => (A, S)

  def map[S, A, B](a: State[S, A])(f: A => B): State[S, B] =
    s1 => {
      val (a2, s2) = a(s1)
      (f(a2), s2)
    }

  def map2[S, A, B, C](a: State[S, A], b: State[S, B])(f: (A, B) => C): State[S, C] =
    s1 => {
      val (a2, s2) = a(s1)
      val (b2, s3) = b(s2)
      (f(a2, b2), s3)
    }

  def flatMap[S, A, B](a: State[S, A])(f: A => State[S, B]): State[S, B] =
    s1 => {
      val (a2, s2) = a(s1)
      f(a2)(s2)
    }

  def unit[S, A](a: A): State[S, A] = s => (a, s)


}

object test extends App {
  val sRng = SimpleRNG(6L)
  println(sRng.double(sRng))
}

