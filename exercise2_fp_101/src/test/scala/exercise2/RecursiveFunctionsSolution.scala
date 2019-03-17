package exercise2

import scala.annotation.tailrec

object RecursiveFunctionsSolution {

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def loop(rem: MyList[A], agg: MyList[A]): MyList[A] = rem match {
      case Nil()         => agg
      case Cons(a, tail) => loop(tail, Cons(a, agg))
    }

    loop(list, Nil())
  }

  def map[A, B](list: MyList[A])(f: A => B): MyList[B] = {
    @tailrec
    def loop(rem: MyList[A], agg: MyList[B]): MyList[B] = rem match {
      case Nil()         => agg
      case Cons(a, tail) => loop(tail, Cons(f(a), agg))
    }

    reverse(loop(list, Nil()))
  }

  def append[A](l: MyList[A], r: MyList[A]): MyList[A] = {
    @tailrec
    def loop(lReversedRem: MyList[A], agg: MyList[A]): MyList[A] = lReversedRem match {
      case Nil()         => agg
      case Cons(a, tail) => loop(tail, Cons(a, agg))
    }

    loop(reverse(l), r)
  }

  def flatMap[A, B](list: MyList[A])(f: A => MyList[B]): MyList[B] = {
    @tailrec
    def loop(rem: MyList[A], agg: MyList[B]): MyList[B] = rem match {
      case Nil()         => agg
      case Cons(a, tail) => loop(tail, append(f(a), agg))
    }

    reverse(loop(list, Nil()))
  }
}
