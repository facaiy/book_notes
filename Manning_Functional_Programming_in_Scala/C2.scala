object C2 {
  // 2.4.1
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, e: (Int, Int)): Int =
      if (n == 0) 0
      else if (n == 1) 1
      else if (n == 2) e._1 + e._2
      else go(n-1, (e._2, e._1 + e._2))

    go(n, (0, 1))
  }

  // 2.5.1
  def findFirst() = {}
}
