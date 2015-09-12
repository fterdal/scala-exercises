/*
 *  Author: Finn Terdal
 *  Date: September 10th, 2015
 *  This exercise asks to implement a setHead function, which
 *  replaces the current head of a list with a different value.
 */

package fpinscala.datastructures

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
    if (as == Nil) Nil
    else Cons(a, List.tail(as))

  // Leftover from previous exercise:
  def drop[A](l: List[A], n: Int): List[A] =
    if (l != Nil && n > 0) drop(List.tail(l), n-1)
    else l

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

}
