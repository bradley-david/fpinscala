
//import scala.{None, Either => _, Option => _, Some => _, _}
// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case (_) => this
  }
  
  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
  
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  
  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }
  
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def seq(l: List[Option[A]], o: Option[List[A]]): Option[List[A]] = (l, o) match {
      case (Nil, _) => o
      case (None :: _, _) => None
      case (Some(h) :: t, Some(x)) => seq(t, Some(h :: x))
    }
    
    seq(a, Some(List[A]())) match {
      case None => None
      case Some(x) => Some(x.reverse)
    }
  }
  
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence_1(t) map (hh :: _))
    }
  
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }
  
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
  
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case (h::t) => map2(f(h),traverse(t)(f))(_ :: _)
  }
  
  
  def map2For[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for{
      aa <- a
      bb <- b
    }yield f(aa,bb)
  
  
}
  
