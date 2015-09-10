/**
 *  Author: Finn Terdal
 *  Date: September 10th, 2015
 *  This program provides a solution to exercise 2.5, which asks
 *  for a higher-order function that composes two given functions.
 */

object MyModule {

  // Leftover from previous exercise
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  // Leftover from previous exercise
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    
  }
}
