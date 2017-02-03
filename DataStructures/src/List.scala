sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    // `List` companion object. Contains functions for creating and working with lists.
    def sum(ints: List[Int]): Int = ints match {
        // A function that uses pattern matching to add up a list of integers
        case Nil => 0 // The sum of the empty list is 0.
        case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }
    
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }
    
    def apply[A](as: A*): List[A] = // Variadic function syntax
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    
    val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
    }
    
    def append[A](a1: List[A], a2: List[A]): List[A] =
        a1 match {
            case Nil => a2
            case Cons(h, t) => Cons(h, append(t, a2))
        }
    
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    
    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x, y) => x + y)
    
    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
    
    
    def tail[A](l: List[A]): List[A] = l match {
        case Nil => sys.error("List is empty.")
        case Cons(_, t) => t
    }
    
    def head[A](l: List[A]): A = l match {
        case Nil => sys.error("List is empty")
        case Cons(x, _) => x
    }
    
    def setHead[A](l: List[A], h: A): List[A] = l match {
        case Nil => sys.error("List is empty.")
        case Cons(_, t) => Cons(h, t)
    }
    
    def drop[A](l: List[A], n: Int): List[A] = {
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)
        }
    }
    
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
    
    // didn't do this one
    def init[A](l: List[A]): List[A] = {
        def go(l: List[A], r: List[A]): List[A] = {
            l
        }
        
        go(l, Nil)
    }
    
    
    def length[A](l: List[A]): Int = {
        foldRight(l, 0)((a, b) => b + 1)
    }
    
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
        l match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }
    }
    
    def foldRightByLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(reverse(l), z)((b, a) => f(a, b))
    
    def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
    
    def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
    
    def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)
    
    def reverse1[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])((a, b) => append(b, List(a)))
    
    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
    
    def appendFold[A](a1: List[A], a2: List[A]): List[A] = {
        foldRightByLeft(a1, a2)((h, t) => Cons(h, t))
    }
    
    def concat[A](l: List[List[A]]): List[A] = {
        foldRightByLeft(l, Nil: List[A])(append)
    }
    
    def add1(l: List[Int]): List[Int] = foldRightByLeft(l, Nil: List[Int])((a, b) => Cons(a + 1, b))
    
    def convertAll(l: List[Double]): List[String] = foldRightByLeft(l, Nil: List[String])((a, b) => Cons(a.toString, b))
    
    def map[A, B](l: List[A])(f: A => B): List[B] = foldRightByLeft(l, Nil: List[B])((a, b) => Cons(f(a), b))
    
    //author's implementation with mutable buffer
    
    def map_2[A, B](l: List[A])(f: A => B): List[B] = {
        val buf = new collection.mutable.ListBuffer[B]
        
        def go(l: List[A]): Unit = l match {
            case Nil => ()
            case Cons(h, t) => buf += f(h); go(t)
        }
        
        go(l)
        List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }
    
    def filter[A](l: List[A])(f: A => Boolean): List[A] = {
        foldRightByLeft(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
    }
    
    //author's implementation with mutable buffer
    
    def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
        val buf = new collection.mutable.ListBuffer[A]
        
        def go(l: List[A]): Unit = l match {
            case Nil => ()
            case Cons(h, t) => if (f(h)) buf += h; go(t)
        }
        
        go(l)
        List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }
    
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))
    
    def filterByMap[A](l: List[A])(f: A => Boolean): List[A] = {
        flatMap(l)(a => if (f(a)) List(a) else Nil)
    }
    
    //better solution with tail recursion
    def addLists(first: List[Int], second: List[Int]): List[Int] = {
        def go(l1: List[Int], l2: List[Int], lf: List[Int]): List[Int] = {
            (l1, l2) match {
                case (Nil, _) => lf
                case (_, Nil) => lf
                case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(h1 + h2, lf))
            }
        }
        
        reverse(go(first, second, List[Int]())) //this solution builds the list backwards, so reverse it
    }
    
    //author's solution with multiple pattern matching
    
    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }
    
    def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
    
    def hasSubsequence[A](list: List[A], sublist: List[A]): Boolean = {
        def go(l: List[A], sub: List[A], o: List[A]): Boolean = (l, sub) match {
            case (_, Nil) => true
            case (Nil, _) => false
            case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) go(t1, t2, o) else go(t1, o, o)
        }
        go(list, sublist, sublist)
    }
    
    def main(args: Array[String]): Unit = {
        println(hasSubsequence(List(1,2,3,4,5),List(7,8)))
    }
}