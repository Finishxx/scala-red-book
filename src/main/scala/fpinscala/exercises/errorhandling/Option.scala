package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match
      case Some(a) => Some(f(a))
      case None    => None

  def getOrElse[B >: A](default: => B): B =
    this match
      case Some(a) => a
      case None    => default

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this match
      case Some(a) if !f(a) => None
      case _                => this

object Option:

  def failingFn(i: Int): Int =
    val y: Int =
      throw new Exception(
        "fail!"
      ) // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch
      case e: Exception =>
        43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception(
        "fail!"
      )): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  // Implement the variance function in terms of flatMap. If the mean of a
  // sequence is m, the variance is the mean of math.pow(x - m, 2) for each
  // element x in the sequence. See the definition of variance on Wikipedia
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(n => math.pow(n - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match
      case (Some(aa), Some(bb)) => Some(f(aa, bb))
      case _                    => None

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List())): (a: A, acc: Option[List[B]]) =>
      map2(f(a), acc)(_ :: _)
