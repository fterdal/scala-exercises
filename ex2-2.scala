/**
 *  Author: Finn Terdal
 *  Date: September 9th, 2015
 *  This very simple program implements a higher-order function,
 *  isSorted. isSorted takes an array of type A (any type), and
 *  returns whether or not the array is sorted, given a function
 *  that compares a pair of As. For sake of example, I've defined
 *  two such functions, intOrdered and stringOrdered. It is also
 *  possible to pass isSorted an anonymous function. 
 */

object MyModule {

  def findFirst(ss: Array[String], key: String) : Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def intOrdered(a: Int, b: Int): Boolean = {  a < b  }

  def stringOrdered(a: String, b: String): Boolean = {  a < b  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n+1 >= as.length) true    // An empty Array is a sorted array
      else if ( !ordered(as(n), as(n+1)) ) false // The array isn't sorted
      else loop(n + 1)    // Check the next two elements
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    //println(findFirst(Array("dog", "cat", "monkey", "elephant"), "cat"))
    //println(stringOrdered("zena","maurice"))
    println(
      isSorted(Array("Alan","Annika","Finn","Ilsa","Kayla"), stringOrdered)
      )
  }
}
