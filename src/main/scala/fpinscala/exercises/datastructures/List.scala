package fpinscala.exercises.datastructures
import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /**
   * Another data constructor, representing nonempty lists. Note that `tail` is
   * another `List[A]`, which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](
    as: List[A],
    acc: B,
    f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
    foldRight(ns, 1.0, _ * _)

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil           => sys.error("Can't do a *tail* of Nil")
      case Cons(_, tail) => tail

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil           => sys.error("Can't do a *setHead* of Nil")
      case Cons(_, tail) => Cons(h, tail)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match
      case Nil           => Nil
      case Cons(_, tail) => if n <= 0 then l else drop(tail, n - 1)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil              => Nil
      case Cons(head, tail) => if f(head) then dropWhile(tail, f) else l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil              => sys.error("Can't do a *init* of Nil")
      case Cons(_, Nil)     => Nil
      case Cons(head, tail) => Cons(head, init(tail))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil              => acc
      case Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (acc, h) => Cons(h, acc))

  // Cheating...
  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(l), acc, (a, b) => f(b, a))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (h, acc) => Cons(h, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], append)

  def incrementEach(l: List[Int]): List[Int] =
    l match
      case Nil              => Nil
      case Cons(head, tail) => Cons(head + 1, incrementEach(tail))

  def doubleToString(l: List[Double]): List[String] =
    l match
      case Nil              => Nil
      case Cons(head, tail) => Cons(head.toString, doubleToString(tail))

  def map[A, B](l: List[A], f: A => B): List[B] =
    l match
      case Nil              => Nil
      case Cons(head, tail) => Cons(f(head), map(tail, f))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil        => Nil
      case Cons(h, t) => if f(h) then Cons(h, filter(t, f)) else filter(t, f)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (_, Nil)                     => Nil
      case (Nil, _)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (_, Nil)                     => Nil
      case (Nil, _)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @tailrec
    def isSubsequence(sup: List[A], sub: List[A]): Boolean =
      (sup, sub) match
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(supHead, supTail), Cons(subHead, subTail)) =>
          supHead == subHead && isSubsequence(supTail, subTail)

    sup match
      case Nil        => sub == Nil
      case Cons(_, t) => isSubsequence(sup, sub) || hasSubsequence(t, sub)
