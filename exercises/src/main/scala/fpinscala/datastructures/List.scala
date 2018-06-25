package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this is the winner
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil          => Cons(h,Nil)
    case Cons(_, Nil) => Cons(h, Nil)
    case Cons(_, xs)  => Cons(h, xs)
  } //

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n<1) l
    else if (n>length(l)) Nil
    else drop(tail(l), n-1) 
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => if (f(x)) dropWhile(xs,f)
                       else Cons(x,xs)
  }
 
  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = {
    def gloop(l: List[A], counter: Int): Int = l match {
      case Nil => counter
      case Cons(_,xs) => gloop(tail(l), counter+1)
    }
    gloop(l, 0)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def main(args: Array[String]): Unit = {
    val p1 = List(1,2,3,4,5)
    val pBl1 = List(2,3,4,5)==tail(p1)
    val p2 = Cons('a', Nil)
    val pBl2 = tail(p2)==Nil
    println("unit test 1 for tail, should return true: " + pBl1)
    println("unit test 2 for tail, should return true: " + pBl2)
    val q1 = List('a','b','c')
    val qBl1 = Cons('z', tail(q1))==setHead(q1,'z')
    println("unit test 1 for setHead, should return true: " + qBl1)
    val r1 = List('a','b','c','d')
    val r2 = List(2,4,6,8,9,10,12)
    val rBl1 = List('c','d')==drop(r1, 2)
    val isEven = (x: Int) => x % 2==0
    val rBl2 = List(9, 10,12)==dropWhile(r2, isEven)
    
    println("unit test1 for drop, should return true: " + rBl1)
    println("unit test1 for dropWhile, should return true: " + rBl2)
  }
}
