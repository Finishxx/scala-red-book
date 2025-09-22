package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import fpinscala.exercises.testing.Result.{Falsified, Passed}

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed          => false
    case Falsified(_, _) => true

opaque type TestCases = Int
object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x

opaque type Prop = (TestCases, RNG) => Result
object Prop:
  opaque type SuccessCount = Int
  opaque type FailedCase = String
  
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    (n, rng) =>
      randomLazyList(a)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try if f(a) then Passed else Falsified(a.toString, i)
            catch case e: Exception => Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Passed)
  
  extension (self: Prop)
    
    def &&(that: Prop): Prop =
      (n, rng) =>
        val res1 = self(n, rng)
        val res2 = that(n, rng)
        if res1.isFalsified then res1 else res2
      
    
    def ||(that: Prop): Prop =
      (n, rng) =>
        val res1 = self(n, rng)
        val res2 = that(n, rng)
        if !res1.isFalsified then res1 else res2
      
    

def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
  LazyList.unfold(rng)(rng => Some(g.run(rng)))

def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
    s"generated and exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

opaque type Gen[+A] = State[RNG, A]

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(number =>
      val distance = (start - stopExclusive).abs
      start + number % distance
    )

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    State(RNG.nonNegativeInt).map(_ % 2 == 0)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(if _ then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    State(RNG.int)
      .map(_.abs.toDouble / Int.MaxValue.toDouble)
      .flatMap(prob =>
        println(s"g1: ${g1._2}, g2: ${g2._2}, prob: $prob")
        if prob <= g1._2 then g1._1 else g2._1
      )

  extension [A](self: Gen[A])
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(n: Int): Gen[List[A]] =
      val intGens: List[Gen[A]] = List.fill(n)(self)
      State.sequence(intGens)

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State: rng =>
        val (value, rng2) = self.next(rng)
        f(value).next(rng2)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(self.listOfN)
