package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def _double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (int, rng2) = rng.nextInt
    (int.abs, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (int, rng2) = nonNegativeInt(rng)
    (int.toDouble / Int.MaxValue, rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (int, rng2) = rng.nextInt
    val (dbl, rng3) = double(rng2)
    ((int, dbl), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((int, dbl), rng2) = intDouble(rng)
    ((dbl, int), rng2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (dbl1, rng2) = double(rng)
    val (dbl2, rng3) = double(rng2)
    val (dbl3, rng4) = double(rng3)
    ((dbl1, dbl2, dbl3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then (Nil, rng)
    else
      val (tail, rngTail) = ints(count - 1)(rng)
      val (int, rngHead) = rngTail.nextInt
      (int :: tail, rngHead)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    // Official solution
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))
    // My solution
    (rng: RNG) =>
      rs match
        case Nil => (Nil, rng)
        case h :: t =>
          val (l, rng2) = sequence(t)(rng)
          val (a, rng3) = h(rng2)
          (a :: l, rng3)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
    f: (A, B) => C
  ): Rand[C] = ???

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
