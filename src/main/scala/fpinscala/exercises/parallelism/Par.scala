package fpinscala.exercises.parallelism

import java.util.concurrent.*

object Par:
  opaque type Par[A] = ExecutorService => Future[A]

  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  /**
   * `unit` is represented as a function that returns a `UnitFuture`, which is a
   * simple implementation of `Future` that just wraps a constant value. It
   * doesn't use the `ExecutorService` at all. It's always done and can't be
   * cancelled. Its `get` method simply returns the value that we gave it.
   */
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  /**
   * `map2` doesn't evaluate the call to `f` in a separate logical thread, in
   * accord with our design choice of having `fork` be the sole function in the
   * API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if
   * we want the evaluation of `f` to occur in a separate thread.
   *
   * This implementation of `map2` does _not_ respect timeouts. It simply passes
   * the `ExecutorService` on to both `Par` values, waits for the results of the
   * Futures `af` and `bf`, applies `f` to them, and wraps them in a
   * `UnitFuture`. To respect timeouts, we'd need a new `Future` implementation
   * that records the amount of time spent evaluating `af`, then subtracts that
   * time from the available time allocated for evaluating `bf`.
   */
  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        val af = pa(es)
        val bf = pb(es)
        UnitFuture(f(af.get, bf.get))

  extension [A](pa: Par[A])
    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        new Future[C]:
          private val futureA = pa(es)
          private val futureB = pb(es)
          @volatile private var cache: Option[C] = None

          def isDone = cache.isDefined
          def get() = get(Long.MaxValue, TimeUnit.NANOSECONDS)

          def get(timeout: Long, units: TimeUnit) =
            val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
            val started = System.nanoTime
            val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
            val elapsed = System.nanoTime - started
            val b = futureB.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
            val c = f(a, b)
            cache = Some(c)
            c

          def isCancelled = futureA.isCancelled || futureB.isCancelled
          def cancel(evenIfRunning: Boolean) =
            futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

  /**
   * This is the simplest and most natural implementation of `fork`, but there
   * are some problems with it--for one, the outer `Callable` will block waiting
   * for the "inner" task to complete. Since this blocking occupies a thread in
   * our thread pool, or whatever resource backs the `ExecutorService`, this
   * implies that we're losing out on some potential parallelism. Essentially,
   * we're using two threads when one should suffice. This is a symptom of a
   * more serious problem with the implementation, and we will discuss this
   * later in the chapter.
   */
  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(
        new Callable[A]:
          def call = a(es).get
      )

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  extension [A](pa: Par[A])
    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    parList.map(_.sorted)

  def sequenceSimple[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight(unit(Nil): Par[List[A]])((par, acc) => par.map2(acc)(_ :: _))

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](pas: List[Par[A]]): Par[List[A]] = ???

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(IndexedSeq(_))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      val ls = sequenceBalanced(l)
      val rs = sequenceBalanced(r)
      ls.map2(rs)(_ ++ _)

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    ???

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork:
      sequence(ps.map(asyncF(f)))

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      val toFilter: Par[List[List[A]]] =
        parMap(l)(el => if f(el) then List(el) else List())

      toFilter.map(list => list.flatten)

  def parallelReduction[A](s: IndexedSeq[A])(z: A)(pls: (A, A) => A): Par[A] =
    if s.size == 1 then unit(s.head)
    else if s.isEmpty then unit(z)
    else
      val (left, right) = s.splitAt(s.size / 2)
      val leftPar = parallelReduction(left)(z)(pls)
      val rightPar = parallelReduction(right)(z)(pls)
      leftPar.map2(fork(rightPar))(pls)

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D) =
    a.map2(b)((_, _)).map2(c)((ab: (A, B), cc: C) => f(ab._1, ab._2, cc))

  def parallelWordCount(paragraphs: IndexedSeq[String]): Par[Int] =

    val sync: String => Par[Int] = asyncF[String, Int](s => s.length)
    val pars = parMap(paragraphs.toList)(_.length)

    // parallelReduction(pars)(unit(0))(_.map2(_)(_ + _))
    ???

    // paragraphs.map(asyncF(s => s.size))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    ???

  /**
   * This is the simplest and most natural implementation of `fork`, but there
   * are some problems with it--for one, the outer `Callable` will block waiting
   * for the "inner" task to complete. Since this blocking occupies a thread in
   * our thread pool, or whatever resource backs the `ExecutorService`, this
   * implies that we're losing out on some potential parallelism. Essentially,
   * we're using two threads when one should suffice. This is a symptom of a
   * more serious problem with the implementation, and we will discuss this
   * later in the chapter.
   */
  def delay[A](fa: => Par[A]): Par[A] = ???

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map(if _ then 0 else 1))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      val i = n(es).get
      choices(i)(es)

  def choiceViaChoiceN[A](
    a: Par[Boolean]
  )(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    ???

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    ???

  extension [A](pa: Par[A])
    def chooser[B](choices: A => Par[B]): Par[B] =
      es => choices(pa(es).get)(es)

  /* `chooser` is usually called `flatMap` or `bind`. */
  extension [A](pa: Par[A])
    def flatMap[B](choices: A => Par[B]): Par[B] =
      chooser(pa)(choices)

  def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    p.flatMap(if _ then t else f)

  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    p.flatMap(choices(_))

  // see nonblocking implementation in `Nonblocking.scala`
  def join[A](a: Par[Par[A]]): Par[A] =
    es => a(es).get.apply(es)

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    a.flatMap(aa => aa)

  extension [A](pa: Par[A])
    def flatMapViaJoin[B](f: A => Par[B]): Par[B] =
      join(pa.map(f))

object Examples:
  import Par.*
  def sum(
    ints: IndexedSeq[Int]
  ): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    ???
