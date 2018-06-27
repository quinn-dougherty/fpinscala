package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //3.25
  def size[A](t: Tree[A]): Int = t match {
    case value => 1
    case Branch(left,right) => size(left) + size(right)
  }

  //3.26
  def maximum(t: Tree[Int]): Int = t match {
    case value => 

  def main(args: Array[String]): Unit = {
    println("TESTINGS for tree methods... ... ...")
  }


}
