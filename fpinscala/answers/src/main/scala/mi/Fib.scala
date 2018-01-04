package mi

object Fib {

  private def fib(n: Int): Int = {
    @annotation.tailrec
    def go(f: Int, s: Int, acc: Int, n: Int): Int = {
      if (n == acc) {
        f
      } else if (n == acc + 1) {
        s
      } else {
        go(s, f + s, acc + 1, n)
      }
    }

    go(0, 1, 1, n)
  }

  def main(args: Array[String]): Unit = {
    (1 to 10).foreach(x => println(fib(x)))
  }

}
