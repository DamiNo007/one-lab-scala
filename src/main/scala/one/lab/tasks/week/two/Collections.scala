package one.lab.tasks.week.two

import scala.annotation.tailrec

import one.lab.tasks.week.one.Operators.{Person, getElderPerson}

object Collections {
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  def getLast[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException()
    case head :: tail => if (tail.isEmpty) head else getLast(tail)
  }

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  def getLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case head :: tail => if (tail.isEmpty) Some(head) else getLastOption(tail)
  }

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException()
    case head :: tail => if (tail.length == 1) head else getPreLast(tail)
  }

  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case head :: tail => if (tail.length == 1) Some(head) else getPreLastOption(tail)
  }

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](n: Int, list: List[A]): A = {
    @tailrec
    def nthElement(list: List[A], acc: Int): A = list match {
      case Nil => throw new NoSuchElementException()
      case head :: tail => {
        if (acc == 0)
          head
        else
          nthElement(tail, acc - 1)
      }
    }

    nthElement(list, n)
  }

  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = {
    @tailrec
    def nthElement(list: List[A], acc: Int): Option[A] = list match {
      case Nil => None
      case head :: tail => {
        if (acc == 0)
          Some(head)
        else
          nthElement(tail, acc - 1)
      }
    }

    nthElement(list, n)
  }

  // getLength(List(1,2,3)) -> 3println("Checking List(1,2,3,4,5)")
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): Int = {
    @tailrec
    def length(list: List[A], acc: Int): Int = list match {
      case Nil => 0
      case head :: tail => if (tail.isEmpty) acc else length(tail, acc + 1)
    }

    length(list, 1)
  }

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = {
    @tailrec
    def reverse(resultList: List[A], reminder: List[A]): List[A] = reminder match {
      case Nil => resultList
      case head :: tail => reverse(head :: resultList, tail)
    }

    reverse(Nil, list)
  }

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = {
    @tailrec
    def duplicateEl(list: List[A], dupList: List[A]): List[A] = list match {
      case Nil => throw new NoSuchElementException()
      case head :: tail => if (tail.isEmpty) dupList ::: makeTwo(head) else duplicateEl(tail, dupList ::: makeTwo(head))
    }

    def makeTwo(n: A): List[A] = n :: List(n)

    duplicateEl(list, List())
  }

  def main(args: Array[String]): Unit = {
    println("Checking List(1,2,3,4,5)")
    val list = List(1, 2, 3, 4, 5) //5
    println(s"Last: ${getLast(list)}")
    println(s"LastOption: ${getLastOption(list)}")
    println(s"PreLast: ${getPreLast(list)}")
    println(s"PreLastOption: ${getPreLastOption(list)}")
    println(s"nth Element: ${getNthElement(2, list)}")
    println(s"nthElementOption: ${getNthElementOption(2, list)}")
    println(s"Length: ${getLength(list)}")
    println(s"Reversed: ${getReversedList(list)}")
    println(s"DuplicateEachEl: ${duplicateEveryElement(list)}")
    println()
    println("Checking List(5,6,7,8,9)")
    val list2 = List(5, 6, 7, 8, 9) //9
    println(s"Last: ${getLast(list2)}")
    println(s"LastOption: ${getLastOption(list2)}")
    println(s"PreLast: ${getPreLast(list2)}")
    println(s"PreLastOption: ${getPreLastOption(list2)}")
    println(s"nth Element: ${getNthElement(2, list2)}")
    println(s"nthElementOption: ${getNthElementOption(2, list2)}")
    println(s"Length: ${getLength(list2)}")
    println(s"Reversed: ${getReversedList(list2)}")
    println(s"DuplicateEachEl: ${duplicateEveryElement(list2)}")
    println()
    println("Checking list of one element List(2)")
    val listOneEl = List(2)
    println(s"Last: ${getLast(listOneEl)}")
    println(s"LastOption: ${getLastOption(listOneEl)}")
    println(s"PreLast: ${getPreLast(listOneEl)}")
    println(s"PreLastOption: ${getPreLastOption(listOneEl)}")
    println(s"nth Element: ${getNthElement(2, listOneEl)}")
    println(s"nthElementOption: ${getNthElementOption(2, listOneEl)}")
    println(s"Length: ${getLength(listOneEl)}")
    println(s"Reversed: ${getReversedList(listOneEl)}")
    println(s"DuplicateEachEl: ${duplicateEveryElement(listOneEl)}")
    println()
    println("Checking List() empty list")
    val emptyList = List()
    println(s"Last: ${getLast(emptyList)}")
    println(s"LastOption: ${getLastOption(emptyList)}")
    println(s"PreLast: ${getPreLast(emptyList)}")
    println(s"PreLastOption: ${getPreLastOption(emptyList)}")
    println(s"nth Element: ${getNthElement(2, emptyList)}")
    println(s"nthElementOption: ${getNthElementOption(2, emptyList)}")
    println(s"Length: ${getLength(emptyList)}")
    println(s"Reversed: ${getReversedList(emptyList)}")
    println(s"DuplicateEachEl: ${duplicateEveryElement(emptyList)}")
    println()
  }
}
