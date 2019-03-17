package exercise2

sealed trait MyList[A]
case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]
case class Nil[A]() extends MyList[A]

/** Write your solutions as stand-alone functions. */
object RecursiveData {

  // a) Implement a function which determines if a `List[Int]` is empty or not.



  // apply your solution-function from (a) here, DO NOT change the signature
  def testListIntEmpty(list: MyList[Int]): Boolean = false

  // b) Implement a function which gets the head of a `List[Int]` or returns -1 if empty.



  // apply your solution-function from (b) here, DO NOT change the signature
  def testListIntHead(list: MyList[Int]): Int = 0

  // c) Can we change `List[A]` to guarantee to be not-empty?


  /* d) Implement a generic Tree which has its values in the leafs and consists of:
   *      node - left and right Tree
   *      leaf - a value of type A
   */

}
