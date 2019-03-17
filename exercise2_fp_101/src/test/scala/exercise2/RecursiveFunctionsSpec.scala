package exercise2

import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._

import scala.annotation.tailrec

object RecursiveFunctionsSpec extends Properties("recursive functions") {

  implicit val listGen = Arbitrary[MyList[Int]] {
    Gen.choose(0, 10).flatMap { length =>
      def generateList(acc: MyList[Int], l: Int): Gen[MyList[Int]] = {
        if (l == 0)
          acc
        else for {
          head <- Gen.posNum[Int]
          list <- generateList(Cons(head, acc), l - 1)
        } yield list
      }

      generateList(Nil(), length)
    }
  }

  val replicationGen = for {
    n <- Gen.choose(0, 10)
    a <- arbitrary[String]
  } yield (n, a)

  property("reverse lists") = forAll { list: MyList[Int] =>
    RecursiveFunctions.testReverse(list) =? RecursiveFunctionsSolution.reverse(list)
  }

  property("map values") = forAll { list: MyList[Int] =>
    RecursiveFunctions.testMap[Int, String](list, _.toString) =? RecursiveFunctionsSolution.map(list)(_.toString)
  }

  property("append lists") = forAll { (l: MyList[Int], r: MyList[Int]) =>
    RecursiveFunctions.testAppend(l, r) =? RecursiveFunctionsSolution.append(l, r)
  }

  property("flatMap values") = forAll { list: MyList[Int] =>
    RecursiveFunctions.testFlatMap[Int, String](list, a => Cons(a.toString, Nil())) =? RecursiveFunctionsSolution.flatMap(list)(a => Cons(a.toString, Nil()))
  }
}
