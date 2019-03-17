package exercise2

import scala.annotation.tailrec

/** Implement stand-alone functions for your solutions.
  * REMARK: Try to make all functions tail recursive; use the annotation to proof it.
  * 
  * A function is tail recursive if if:
  *   1. it is single direct recursion
  *   2. the last expression is the recursive call
  */
object RecursiveFunctions {

  def length[A](as: MyList[A]): Int = {
    @tailrec
    def loop(rem: MyList[A], agg: Int): Int = rem match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }

    loop(as, 0)
  }

  /* a) Write a function that reverses a list:
   *        def reverse[A](list: List[A]): List[A]
   */



  // apply your solution-function from (a) here, DO NOT change the signature
  def testReverse[A](list: MyList[A]): MyList[A] = list

  /* b) Write a function that applies a function to every value of a list:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */



  // apply your solution-function from (b) here, DO NOT change the signature
  def testMap[A, B](list: MyList[A], f: A => B): MyList[B] = Nil()
  
  /* c) Write a function that appends one list to another:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */



  // apply your solution-function from (c) here, DO NOT change the signature
  def testAppend[A](l: MyList[A], r: MyList[A]): MyList[A] = l

  /* d) Write a function that applies a function to every value of a list:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   * 
   *    it gets a function which creates a new List[B] for every element of type A in 
   *    the list. Therefore, you generate a List[List[B]]. You have to flatten this 
   *    structure.
   */



  // apply your solution-function from (d) here, DO NOT change the signature
  def testFlatMap[A, B](list: MyList[A], f: A => MyList[B]): MyList[B] = Nil()

  /* e) Question: Is it possible to write a tail recursive map function for `Tree`s? If no, why and are you sure :) ? */
}
