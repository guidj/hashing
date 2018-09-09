package fpinscala.laziness

trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], acc: Stream[A], c: Long): Stream[A] = s match {
      case Empty => acc
      case Cons(h, t) =>
        if (c == 0) {
          acc
        } else {
          go(t(), Cons(h, () => acc), c - 1)
        }
    }

    go(go(this, Empty, n), Empty, n)
  }

  def drop(n: Int): Stream[A] = {
    // keep iterating while n is > 0, then return stream
    def go(s: Stream[A], n: Int): Stream[A] = s match {
      case Empty => s
      case Cons(h, t) =>
        if (n == 0){
          Cons(h, t)
        } else {
          go(t(), n - 1)
        }
    }

    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){
      case (a, as) =>
        if (p(a)){
          Stream.cons(a, as.takeWhile(p))
        } else {
          Empty
        }
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(false)((a, as) => p(a) || as)
  }

  def headOption: Option[A] = {
    lazy val z: Option[A] = None
    foldRight(z)((a, o) => Option(a).orElse(o))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B]){
      case (a, as) => Stream.cons(f(a), as)
    }
  }

  def filter[B](f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){
      case (a, as) =>
        if (f(a)) {
          Stream.cons(a, as)
        } else {
          as
        }
    }
  }

//  def append(a: => A): Stream[A] = {
//    lazy val x = a()
//    foldRight(Stream.cons(x, Stream.empty[A]))((a, as) => Stream.cons(a, as))
//  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B]){
      case (a, as) =>
        f(a).foldRight(as){
          case (b, bs) =>
            Stream.cons(b, bs)
        }
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = {
    def go(s: Stream[A], h: List[A]): List[A] = s match {
      case Empty => h
      case Cons(head, tail) =>
        go(tail(), head() :: h)
    }

    go(this, Nil).reverse
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val   tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def optionToStream[A](v: Option[A]): Stream[A] = {
    v.map(x => Stream.cons(x, Stream.empty[A])).getOrElse(Stream.empty[A])
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  def main(args: Array[String]): Unit = {
    val s = Stream(1, 2, 3, 4, 5)
    val ss = Stream(Stream(1, 2, 3), Stream(4, 5, 6), Stream(7, 8, 9))
    val so = Stream(Option(1), None, Option(2))
    printf("s.toList: %s\n", s.toList)
    printf("s.take(3).toList: %s\n", s.take(3).toList)
    printf("s.drop(3).toList: %s\n", s.drop(3).toList)
    printf("s.takeWhile(_ < 3).toList: %s\n", s.takeWhile(_ < 3).toList)
    printf("s.headOption: %s\n", s.headOption)
    printf("Stream.empty.headOption: %s\n", Stream.empty.headOption)
    printf("s.map(_ * 2).toList: %s\n", s.map(_ * 2).toList)
    printf("s.filter(_ mod 2 == 0).toList: %s\n", s.filter(_ % 2 == 0).toList)
    printf("ss.map(_.toList).toList: %s\n", ss.map(_.toList).toList)
    printf("ss.flatMap(identity).toList: %s\n", ss.flatMap(identity).toList)
    printf("so.toList: %s\n", so.toList)
    printf("so.flatMap(optionToStream).toList: %s\n", so.flatMap(optionToStream).toList)
  }
}