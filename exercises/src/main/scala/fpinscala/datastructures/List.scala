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
// this version of product2, because foldright, can't skip to the end and make a "0" on the encounter of its first zero. 


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
  }

  def head[A](l: List[A]): A = l match {
    //case Nil => error
    case Cons(x, _) => x
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

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_,Nil) => Nil
    case Cons(x,xs)  => Cons(x,init(xs))
  }

  def length[A](l: List[A]): Int = {
    def gloop(l: List[A], counter: Int): Int = l match {
      case Nil => counter
      case Cons(_,xs) => gloop(tail(l), counter+1)
    }
    gloop(l, 0)
  }

  def length2[A](l: List[A]): Int = {
    foldRight(l,0)((_:A, x:Int) => x+1)
  }

// this must be made tail-recursive (exercise 3.10)
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x,Nil) => f(z,x)
    case Cons(x,xs) => f(foldLeft(xs,z)(f),x)
  }

  def sum3(l: List[Int]): Int = foldLeft(l,0)(_+_)
  def product3(l: List[Int]): Int = foldLeft(l,1)(_*_)
  def length3[A](l: List[A]): Int = { 
    foldLeft(l,0)((x:Int, _:A) => x+1)
  }

// exercises 3.12
  def reverse(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x,xs) => append(reverse(xs), Cons(x,Nil))
  }


// exercise 3.13(HARD) -- implement foldLeft in terms of foldRight, vice versa. -- hint, this is what helps w tailrecursion.

// exercise 3.14 -- immplement append in terms of either foldLeft or foldRight. 

// 3.15 -- concat: [[A]]->[A]
  def concat[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(xs,xss) => append(xs,concat(xss))
  }

  //3.16
  def incrOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x,xs) => Cons(x+1,incrOne(xs))
  }

// 3.17
  def doubToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x,xs) => Cons(x.toString,doubToString(xs))
  }

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x,xs) => Cons(f(x),map(xs)(f))
  }

  // 3.19
  def filter[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => if (p(x)) Cons(x,filter(xs)(p)) else filter(xs)(p)
  }

  //3.20  //
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x,xs) => append(f(x),(flatMap(xs)(f))) // apparently this works. and you oughtn't say 
  }
/* 
  //3.21 do filter w flatmap
  def filter[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => // stil don't have it
  } */ 

  // 3.22 do "zip with plus" on (ls: List[Int])(ms: List[Int])
  def zwp(as: List[Int], bs: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(x,xs) => Cons(x+(head(bs)),zwp(xs,tail(bs)))
    // fails when length as is greater than length bs. 
  }
//    = Cons(head(as)+head(bs),zwp(tail(as),tail(bs))) 


      // 3.23 generalize it properly into zipwith.
  def zipWith[A,B,C](as: List[A])(bs: List[B])(op: (A,B) => C): List[C] =
    (as,bs) match {
      case (Nil,Nil) => Nil
      case (_  ,Nil) => Nil
      case (Nil,_  ) => Nil
      case (Cons(x,xs),Cons(y,ys)) => Cons(op(x,y),zipWith(xs)(ys)(op))
    }

//3.24 Hard -- hasSubsequence, check whether list has a sublist
// hasSubsequence(List(1,2,3,4))(List(1,2)) ==> true

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
    val tBl1 = init(r1)==List('a','b','c')
    println("unit test1 for drop, should return true: " + rBl1)
    println("unit test1 for dropWhile, should return true: " + rBl2)
    println("unit test1 for init, should return true: " + tBl1)
    val uBl1 = List(3,5,7,9,10,11,13) == (map(r2)((x: Int)=>x+1))
    println("test1 for map, should return true: " + uBl1)

    println("TESTING FOR FOLDLEFT")
    val plus = (x:Int, y:Int) => x+y
    println(r2)
    println("folding r2 w + (left)")
    println(foldLeft(r2,0)(plus))
    println("folding r2 w + (right)") // hm am i getting the left vs. right correct? 
    println(foldRight(r2,0)(plus))

    // EXERCISE 3.8
    println("exercise 3.8: ") 
    println(foldRight(List(1,2,3),Nil:List[Int])(Cons(_,_)))
    println("should be id- should return List(1,2,3)") // that is correct
    val vBl1 = 4==length2(r1)
    println("test for length2, which uses foldRight; should return true: " + vBl1)

    val wBl01 = sum3(List(1,2,3))==product3(List(1,2,3))
    val wBl11 = sum3(List(1,2,3))==length3(List(1,2,3,4,5,6))
    val wBl1 = wBl01==wBl11
    println("testing the foldLeft functions, should say true: " + wBl1)
    val xBl1 = reverse(p1)==List(5,4,3,2,1)
    println("testing reverse, should say true: " + xBl1)

    val yBl1 = flatMap(List(1,2,3))(i => List(i,i))==List(1,1,2,2,3,3)
    println("testing flatmap, should say true: " + yBl1)
    val zBl1 = p1==concat(List(List(1),List(2),List(3),List(4),List(5)))
    println("testing concat, should say true: " + zBl1)

    val aBl1 = filter(p1)(isEven)==List(2,4)
    println("testing filter, should say true: " + aBl1)
/*
    //println(zwp(r2,p1)) broken because length r2 > length p1
    println(zwp(p1,r2))
    println(zipWith(r2)(p1)(_+_))
    println(zipWith(p1)(r2)(_+_)) */ 
    val bBl1 = zipWith(r2)(p1)(_+_)==zwp(p1,r2)
    println("testing zipWith, should say true: " + bBl1)
  }
}


/// SYNTAX NOTE: "(+)" in haskell is "_ + _" in Scala
