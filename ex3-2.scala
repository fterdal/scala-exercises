/*
 *  Author: Finn Terdal
 *  Date: September 10th, 2015
 *  This exercise asks to implement a tail function, which returns
 *  the tail of a given list (i.e. the list excluding the head).
 *
 */

//package fpinscala.datastructures

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

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case Cons(_,t) => t
  }

}
