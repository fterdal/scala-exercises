/**
 *  Author: Finn Terdal
 *  Date: September 10th, 2015
 *  This program provides a solution to exercise 2.3, which asks
 *  for an implementation to the uncurrying function. There is only
 *  one solution that compiles, so this should be it.
 */

object MyModule {

  // Leftover from previous exercise
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def main(args: Array[String]): Unit = {
    
  }
}
