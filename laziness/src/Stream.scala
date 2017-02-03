package fpinscala.laziness.src

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }
  
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  
  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  @annotation.tailrec
  final def exists_tail(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists_tail(p)
    case _ => false
  }
  
  def toList: List[A] = foldRight(Nil: List[A])((a, b) => a :: b)
  
  def toList_1: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    
    go(this, List()).reverse
  }
  
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }
  
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) => if (n > 0) t().drop(n - 1) else this
    case (_) => Stream empty
  }
  
  
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Stream cons(h(), t() takeWhile p)
    case (_) => Stream.empty
  }
  
  def takeWhile_1(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => Stream cons(h(), t() takeWhile_1 f)
    case _ => Stream empty
  }
  
  @tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }
  
  def takeWhileFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a: A, b) => if (f(a)) cons(a, b) else empty)
  
  
  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))
  
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))
  
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))
  
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)
  
  def map_unfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }
  
  def take_unfold(n: Int) = unfold((this, n)) {
    case (Cons(h, t), x) => if (n > 1) Some((h(), (t(), n - 1))) else Some((h(), (empty, 0)))
    case _ => None
  }
  
  def takeWhile_unfold(f: A => Boolean) = unfold(this) {
    case Cons(h, t) => if (f(h())) Some((h(), t())) else None
  }
  
  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b)) {
    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
  }
  
  
  //from the authors
  //I have no idea what the hell this "->" thing is
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))
  
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }
  
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s) forAll {
      case ((Some(a), Some(b))) => a == b
      case ((None, Some(b))) => false
      case (_, None) => true
    }
  }
  
  //from authors
  def startsWith_1[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }
  
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)
  
  /*The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
  
  
}


case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  
  val ones: Stream[Int] = Stream.cons(1, ones)
  
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  
  def constant_1[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  
  def fibs: Stream[Int] = {
    def fib(f1: Int, f2: Int): Stream[Int] = {
      cons(f1, fib(f2, f1 + f2))
    }
    
    fib(0, 1)
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  
  val fibsViaUnfold =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }
  
  def from_unfold(n: Int) = unfold(n)(x => Some((x, x + 1)))
  
  def constant_unfold[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))
  
  val ones_unfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
  
  
}

object TestStuff {
  def main(args: Array[String]) = {
    println(Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty))).toList)
    println(Stream(1, 2, 3).drop(2).toList)
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9) takeWhile_1 (x => x < 4) toList)
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9) takeWhile_unfold (x => x < 4) toList)
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9) takeWhileFoldRight (_ < 4) toList)
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9) drop 3 toList)
    println(Stream(1, 2, 3, 4, 5, 6) startsWith (Stream(1, 2, 3)))
    println(Stream(1, 2, 3, 4).tails flatMap (x => x) toList)
    
  }
}