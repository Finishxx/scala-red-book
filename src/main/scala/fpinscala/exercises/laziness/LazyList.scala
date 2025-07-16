package fpinscala.exercises.laziness
import fpinscala.exercises.laziness.LazyList.{empty, cons, unfold}

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    this match
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) =>
        // If `f` doesn't evaluate its second argument, the recursion never occurs.
        f(h(), t().foldRight(z)(f))
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, _) if n == 1 => cons(h(), Empty)
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case _                    => Empty

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (_, 0)          => None
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
      case _               => None

    }

  def drop(n: Int): LazyList[A] =
    this match
      case Cons(_, t) if n >= 1 => t().drop(n - 1)
      case _                    => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    return takeWhileViaFoldRight(p)
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => Empty

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => if p(h()) then Some(h(), t()) else None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true): (a, acc) =>
      p(a) && acc

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty[A]): (a, acc) =>
      if p(a) then cons(a, acc) else Empty

  def headOption: Option[A] =
    foldRight(None: Option[A]): (a, _) =>
      Some(a)

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B]): (a, acc) =>
      cons(f(a), acc)

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A]): (a, acc) =>
      if f(a) then cons(a, acc) else acc

  def append[A2 >: A](what: => LazyList[A2]): LazyList[A2] =
    foldRight(what): (a, acc) =>
      cons(a, acc)

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B]): (a, acc) =>
      f(a).append(acc)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Empty, _)                   => None
      case (_, Empty)                   => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this, that) {
      case (Empty, Empty)      => None
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }

  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll((a1, a2) => a1 == a2)
  /*zipAll(s).forAll {
        case (Some(a), Some(b)) => a == b
        case (Some(_), None)    => true
        case _                  => false
      }*/

  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some(Cons(h, t), t())
    }.append(LazyList(empty))

  def scanRight[B](z: => B)(f: (A, => B) => B): LazyList[B] =
    foldRight(LazyList(z)): (a, acc) =>
      acc match
        case Empty      => ???
        case Cons(h, _) => cons(f(a, h()), acc)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def fib(n2: Int, n1: Int): LazyList[Int] =
      val n = n2 + n1
      cons(n, fib(n1, n))
    cons(0, cons(1, fib(0, 1)))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None         => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    def fib(n2: Int, n1: Int): Option[(Int, (Int, Int))] =
      val n = n2 + n1
      Some((n, (n1, n)))
    cons(0, cons(1, unfold((0, 1))(fib)))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(num => Some(num, num + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(a => Some((a, a)))

  lazy val onesViaUnfold: LazyList[Int] =
    continuallyViaUnfold(1)
