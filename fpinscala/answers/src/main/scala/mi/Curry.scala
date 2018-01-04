package mi


object Curry {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    def g(a: A)(b: B): C = {
      f(a, b)
    }
    g
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    def g(a: A, b: B): C = {
      f(a)(b)
    }
    g
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    def h(a: A): C = {
      f(g(a))
    }
    h
  }

  def main(args: Array[String]): Unit = {

  }
}
