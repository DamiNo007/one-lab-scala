package one.lab.tasks.week.three

import one.lab.demos.week.two.Demos.Empty
import one.lab.tasks.week.one.Recursion.printNTimes

import scala.annotation.tailrec

object Collections {

  // duplicateNTimes(3, List(1,2,3)) == List(1,1,1,2,2,2,3,3,3)
  // duplicateNTimes(3, List()) == List()
  def duplicateNTimes[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def dup(list: List[A], dupList: List[A]): List[A] = list match {
      case Nil => throw new NoSuchElementException()
      case head :: tail => if (tail.isEmpty) dupList ::: makeN(n, head) else dup(tail, dupList ::: makeN(n, head))
    }

    def makeN(n: Int, num: A): List[A] = {
      @tailrec
      def nTimes(m: Int, dupList: List[A]): List[A] = m match {
        case 0 => dupList
        case _ => nTimes(m - 1, num :: dupList)
      }

      nTimes(n - 1, List(num))
    }

    dup(list, List())
  }

  //Implemented with a flatmap here
  def duplicateNTimesUsingFlatMap[A](n: Int, list: List[A]): List[A] = {
    def makeN(n: Int, num: A): List[A] = {
      @tailrec
      def nTimes(m: Int, dupList: List[A]): List[A] = m match {
        case 0 => dupList
        case _ => nTimes(m - 1, num :: dupList)
      }

      nTimes(n - 1, List(num))
    }

    list flatMap { l => makeN(n, l) }
  }

  // splitAtK(4, List(1,2,3,4,5,6,7,8,9)) == (List(1,2,3,4), List(5,6,7,8,9))
  // splitAtK(0, List(1,2,3)) == (List(), List(1,2,3))
  def splitAtK[A](k: Int, list: List[A]): (List[A], List[A]) = {
    @tailrec
    def split(n: Int, list: List[A], remainder: List[A]): (List[A], List[A]) = (n, list) match {
      case (_, Nil) => (remainder, Nil)
      case (0, l) => (remainder, l)
      case (num, head :: tail) => split(num - 1, tail, remainder :+ head)
    }

    split(k, list, Nil)
  }

  // removeKthElement(5, List(1,2,3,4,5,6)) == (List(1,2,3,4,5), 6)
  // removeKthElement(2, List(1,2,3,4,5,6)) == (List(1,2,4,5,6), 3)
  // removeKthElement(-3, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  // removeKthElement(1000, List(1,2,3,4,5,6)) == IndexOutOfBoundException

  def removeKthElement[A](k: Int, list: List[A]): (List[A], A) = {
    if (k < 0) throw new IndexOutOfBoundsException

    @tailrec
    def remove(list: List[A], beforeK: List[A], i: Int, k: Int): (List[A], A) = list match {
      case Nil => throw new IndexOutOfBoundsException
      case x :: tail => if (i == k) ((beforeK ::: tail), x) else remove(tail, (beforeK :+ x), i + 1, k)
    }

    remove(list, List(), 0, k)
  }

  def main(args: Array[String]): Unit = {
    println("Checking duplicate methods")
    println(duplicateNTimes(3, List(1, 2, 3)) == List(1, 1, 1, 2, 2, 2, 3, 3, 3))
    println(duplicateNTimesUsingFlatMap(3, List()) == List())
    println("Checking split method")
    println(splitAtK(4, List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == (List(1, 2, 3, 4), List(5, 6, 7, 8, 9)))
    println(splitAtK(0, List(1, 2, 3)) == (List(), List(1, 2, 3)))
    println("Checking remove method")
    println(removeKthElement(5, List(1, 2, 3, 4, 5, 6)) == (List(1, 2, 3, 4, 5), 6))
    println(removeKthElement(2, List(1, 2, 3, 4, 5, 6)) == (List(1, 2, 4, 5, 6), 3))
    println(removeKthElement(-3, List(1, 2, 3, 4, 5, 6)))
    println(removeKthElement(1000, List(1, 2, 3, 4, 5, 6)))
  }
}
