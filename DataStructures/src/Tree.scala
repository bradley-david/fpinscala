
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
    
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(a) => 1
        case Branch(left, right) => size(left) + size(right) + 1
    }
    
    def max(t: Tree[Int]): Int = t match {
        case Leaf(i) => i
        case Branch(left, right) => max(left) max max(right)
    }
    
    def depth[A](tree: Tree[A]): Int = {
        
        def go(t: Tree[A], d: Int): Int = t match {
            case Leaf(_) => d
            case Branch(left, right) => go(left, d + 1) max go(right, d + 1)
        }
        
        go(tree, 1)
    }
    
    //better implementation from authors
    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
    
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    
    def fold[A, B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
        case Leaf(x) => f(x)
        case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
    }
    
    def sizeByFold[A](t: Tree[A]) = fold(t)(a => 1)((l, r) => l + r + 1)
    
    def maxByFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
    
    def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
    
}