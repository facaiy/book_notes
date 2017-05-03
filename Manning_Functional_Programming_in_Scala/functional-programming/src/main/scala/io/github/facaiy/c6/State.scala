package io.github.facaiy.c5

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng1) = rng.nextInt

    if (n == Int.MinValue) nonNegativeInt(rng1)
    else (n.abs, rng1)
  }

  // ex 6.2
  /*
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng1) = nonNegativeInt(rng)

    (n / (Int.MaxValue.toDouble + 1), rng1)
  }
  */

  // ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)

    ((n, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (n, rng2) = rng1.nextInt

    ((d, n), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  // ex 6.4
  /*
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val rs = Range(1, count).scanLeft((0, rng)){ case ((_, r), _) => r.nextInt }

    (rs.map(_._1).toList, rs.last._2)
  }
  */

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng1) = s(rng)

      (f(a), rng1)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(x => x - x % 2)

  // ex 6.5
  def double: Rand[Double] = map(nonNegativeInt)(x => x / Int.MaxValue.toDouble + 1)

  // ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (aa, rng1) = ra(rng)
      val (bb, rng2) = rb(rng1)

      (f(aa, bb), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def intDouble: Rand[(Int, Double)] = both(int, double)
  def doubleInt: Rand[(Double, Int)] = both(double, int)

  // ex 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      fs.foldRight((List.empty[A], rng)){ case (f, (es, r)) =>
        val (x, rng1) = f(r)

        (x :: es, rng1)
      }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
}

/*
object RNG {

  // ex 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)

      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ x =>
    val mod = x % n
    if (x + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  // ex 6.9
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(unit(_))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => rng0 => flatMap(rb)(b => rng1 => (f(a, b), rng1)))
}


case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(unit(_))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.flatMap(b => State(r => (f(a, b), r))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    val r = s: S =>{
      val a, s1 = run(s)
      f(a).run(s1)
    }

    State(r)
  }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(State(s => (List.empty, s)))((x, z) => x.map2(z)(_ :: _))
}

// ex 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
*/
