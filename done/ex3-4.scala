/*
 *  Author: Finn Terdal
 *  Date: September 10th, 2015
 *  This exercise asks to implement a drop function, which
 *  drops a specified number of elements from the front of a
 *  singly linked list.
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

  // Leftover from previous exercise:
  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  // Leftover from previous exercise:
  def setHead[A](as: List[A], a: A): List[A] =
    Cons(a, List.tail(as))
    
  def drop[A](l: List[A], n: Int): List[A] =
    if (l != Nil && n > 0) drop(List.tail(l), n-1)
    else l

}
