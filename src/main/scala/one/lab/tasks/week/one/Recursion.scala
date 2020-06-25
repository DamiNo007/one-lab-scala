package one.lab.tasks.week.one

object Recursion {
  def printNTimes(n: Int, value: String): Unit = {
    if (n == 0) return
    print(s"$value ")
    printNTimes(n - 1, value)
  }

  def gcd(a: Long, b: Long): Long = {
    if (b == 0) a else gcd(b, a % b)
  }

  def nthFibonacciNumber(n: Int): Int = {
    if (n <= 0) 0
    else if (n <= 1) 1
    else nthFibonacciNumber(n - 1) + nthFibonacciNumber(n - 2)
  }

  def tailRecursiveFibonacciNumber(n: Int): Int = {
    def tailRecFib(n: Int, prev: Int, curr: Int): Int = {
      if (n <= 0) curr
      else tailRecFib(n - 1, prev = prev + curr, curr = prev)
    }

    tailRecFib(n, 1, 0)
  }

  def main(args: Array[String]): Unit = {
    printNTimes(3, "Damir")
  }
}
