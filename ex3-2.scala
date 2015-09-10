/*
 *  Author: Finn Terdal
 *  Date: September 10th, 2015
 *  This exercise asks to implement a tail function, which returns
 *  the tail of a given list (i.e. the list excluding the head).
 *  the code before 
 *
 */

package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def main(args: Array[String]) = {
    println("Hello, World!")
  }


}
