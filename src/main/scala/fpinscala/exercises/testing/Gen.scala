package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

trait Prop:
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

object Prop:
  opaque type SuccessCount = Int
  opaque type FailedCase = String
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State: rng =>
        val (a, rng2) = self.next(rng)
        f(a).next(rng2)

    def listOfN(gen: Gen[Int]): Gen[List[A]] =
      gen.flatMap(listOfN)

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map: num =>
      start + (num % (start - stopExclusive))

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    choose(Int.MinValue, Int.MaxValue).map(_ <= 0)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if _ then g1 else g2)

/*

trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
 */
