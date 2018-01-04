package mi

import scala.annotation.tailrec


sealed trait SList[+A]

case object Nil extends SList[Nothing]

case class Cons[+A](head: A, tail: SList[A]) extends SList[A]

object SList {
  def sum(ints: SList[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: SList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): SList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def trail[A](as: SList[A]): Option[SList[A]] = {

    as match {
      case Nil => None
      case Cons(_, tail) => Some(tail)
    }

    //Nill: (Nill.. infinite recursion); throw exception (not functional);
    // None (making tail an option)
  }

  def setHead[A](a: A, as: SList[A]): SList[A] = {
    as match {
      case Nil => Cons(a, Nil)
      case Cons(_, tail) => Cons(head = a, tail = tail)
    }
  }

  //TODO: implement flatMap with and without explicit trail recursion; for compreshension for filtering or tail recursion


  def drop[A](l: SList[A], n: Int): SList[A] = {
    //on zero return tail
    //otherwise, send tail and keep iterating

    if (n < 0) {
      l
    } else {
      @annotation.tailrec
      def go(l: SList[A], c: Int): SList[A] = {

        l match {
          case Nil => Nil
          case Cons(_, tail) =>
            if (c == 0) {
              tail
            } else {
              go(tail, c - 1)
            }
        }
      }

      go(l, n - 1)
    }
  }

  def dropWhile[A](l: SList[A])(f: A => Boolean): SList[A] = {
    @annotation.tailrec
    def go(l: SList[A], f: A => Boolean): SList[A] = {
      l match {
        case Nil => Nil
        case Cons(h, t) =>
          if (!f(h)) {
            Cons(h, t)
          } else {
            go(t, f)
          }
      }
    }

    go(l, f)
  }


  def foldRight[A, B](as: SList[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  @annotation.tailrec
  def foldLeft[A, B](as: SList[A], z: B)(g: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, g(z, h))(g)
    }

  //  def foldLeft[A, B](as: SList[A], z: B)(f: (B, A) => B): B = {
  //
  //    @annotation.tailrec
  //    def go(as: SList[A], acc: B): B = {
  //      as match {
  //        case Nil => acc
  //        case Cons(h, t) => go(t, f(acc, h))
  //      }
  //    }
  //
  //    go(as, z)
  //  }

  def sumRight(as: SList[Int]): Int = {
    foldRight(as, 0)(_ + _)
  }

  def productRight(as: SList[Double]): Double = {
    foldRight(as, 1.0)(_ * _)
  }

  def lengthRight[A](as: SList[A]): Int = {
    foldRight(as, 0)((_, b) => 1 + b)
  }

  def sumLeft(as: SList[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def productLeft(as: SList[Double]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }

  def lengthLeft[A](as: SList[A]): Int = {
    foldLeft(as, 0)((b, _) => 1 + b)
  }

  def reverse[A](as: SList[A]): SList[A] = {
    foldLeft(as, Nil: SList[A])((b, a) => Cons(a, b))
  }

  def foldRightViaFoldLeft[A, B](as: SList[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((a: A, b: B) => f(b, a))
  }


  //  def foldRightViaFoldLeft[A, B](as: SList[A], z: B)(f: (B, A) => B): B = {
  //    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  //  }
  //
  //  def foldLeftViaFoldRight[A, B](l: SList[A], z: B)(f: (B, A) => B): B =
  //    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // f(3, f(2, f(1, z)))

  //f(1, f(2, f(3, f(4, z)))))
  def append[A](a: A, as: SList[A]): SList[A] = {
    foldRight(as, Cons(a, Nil))((a, b) => Cons(a, b))
  }

  def concatenate[A](as: SList[SList[A]]): SList[A] = {

    foldRight(as, Nil: SList[A])((a, b) => {
      foldRight(a, b)((aa, bb) => Cons(aa, bb))
    })
  }

  def addone(as: SList[Int]): SList[Int] = {
    foldRight(as, Nil: SList[Int])((a, b) => Cons(a + 1, b))
  }

  def stringify(as: SList[Double]): SList[String] = {
    foldRight(as, Nil: SList[String])((a, b) => Cons(a.toString, b))
  }

  def map[A, B](as: SList[A])(f: A => B): SList[B] = {
    foldRight(as, Nil: SList[B])((a, b) => Cons(f(a), b))
  }

  def filter[A](as: SList[A])(f: A => Boolean): SList[A] = {
    foldRight(as, Nil: SList[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  def flatMap[A, B](as: SList[A])(f: A => SList[B]): SList[B] = {
    concatenate(map(as)(f))
  }

  def filterViaFlatMap[A](as: SList[A])(f: A => Boolean): SList[A] = {
    flatMap(as)(a => if (f(a)) Cons(a, Nil: SList[A]) else Nil: SList[A])
  }

  def add(xs: SList[Int], ys: SList[Int]): SList[Int] = {

    @annotation.tailrec
    def go(xs: SList[Int], ys: SList[Int], zs: SList[Int]): SList[Int] = {
      (xs, ys) match {
        case (Nil, Nil) => zs
        case (Cons(_, _), Nil) => zs
        case (Nil, Cons(_, _)) => zs
        case (Cons(hA, tA), Cons(hB, tB)) => go(tA, tB, Cons(hA + hB, zs))
      }
    }

    reverse(go(xs, ys, Nil: SList[Int]))
  }


  def zipWith[A, B](xs: SList[A], ys: SList[A])(f: (A, A) => B): SList[B] = {

    @annotation.tailrec
    def go(xs: SList[A], ys: SList[A], zs: SList[B]): SList[B] = {
      (xs, ys) match {
        case (Nil, Nil) => zs
        case (Cons(_, _), Nil) => zs
        case (Nil, Cons(_, _)) => zs
        case (Cons(hA, tA), Cons(hB, tB)) => go(tA, tB, Cons(f(hA, hB), zs))
      }
    }

    reverse(go(xs, ys, Nil: SList[B]))
  }

  def hasSubsequence[A](sup: SList[A], sub: SList[A]): Boolean = {

    def go(sup: SList[A], sub: SList[A], matched: Boolean, headSub: SList[A], pivot: SList[A]): Boolean = {
      (sup, sub) match {
        case (Nil, Nil) => matched
        case (Nil, Cons(_, _)) => false
        case (Cons(_, _), Nil) => matched
        case (Cons(hSup, tSup), Cons(hSub, tSub)) =>

          if (hSup == hSub) {
            if (!matched) {
              go(tSup, tSub, true, headSub, pivot = Cons(hSup, tSup))
            } else {
              go(tSup, tSub, true, headSub, pivot)
            }
          } else {
            go(tSup, headSub, false, headSub, pivot = tSup)
          }
      }
    }

    go(sup, sub, false, headSub = sub, pivot = sup)
  }

  def main(args: Array[String]): Unit = {
    val x = SList(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + SList.sum(t)
      case _ => 101
    }

    println(x)

    println(SList.drop(SList(1, 2, 3, 4, 5), -3))
    println(SList.drop(SList(1, 2, 3, 4, 5), 2))
    println(SList.drop(SList(1, 2, 3, 4, 5), 5))
    println(SList.drop(SList(1, 2, 3, 4, 5), 20))
    println(List(1, 2, 4).drop(-1))

    println(SList.dropWhile(SList(1, 2, 3, 4, 5))(v => v < 5))

    println("Fold right: %s".format(SList.foldRight(SList(1, 2, 3), Nil: SList[Int])(Cons(_, _))))

    println("Length with foldRight: %s".format(lengthRight(SList(1, 2, 3, 4, 5))))

    println(reverse(SList(1, 2, 3, 4, 5)))
    println("Fold right (using foldleft): %s".format(
      SList.foldRightViaFoldLeft(SList(1, 2, 3), Nil: SList[Int])((b, a) => Cons(a, b))
    ))

    println("Append: %s".format(append(6, SList(1, 2, 3, 4, 5))))
    println("Concat: %s".format(concatenate(SList(SList(1, 2, 3), SList(10, 11, 12)))))
    println("AddOne: %s".format(addone(SList(1, 2, 3, 4))))
    println("Stringify: %s".format(stringify(SList(1, 2, 3, 4))))
    println("Map: %s".format(map(SList(1, 2, 3, 4))(_ * 3)))
    println("Filter even: %s".format(filter(SList(1, 2, 3, 4))(_ % 2 == 0)))
    println("Filter via flatMap: %s".format(filterViaFlatMap(SList(1, 2, 3, 4))(_ % 2 == 0)))
    println("Add lists: %s".format(add(SList(1, 2, 3, 4), SList(1, 2, 3, 4))))
    println("Addition using zipWith: %s".format(
      zipWith(SList(1, 2, 3, 4), SList(1, 2, 3, 4))(_ + _)
    ))

    val lists = SList(
      (SList(1, 2, 3, 4, 5), SList(2, 3)),
      (SList(1, 2, 3), SList(9, 10)),
      (SList(1, 2, 3), SList(1)),
      (SList(1, 2, 3), SList(3))
    )

    map(lists)(sListPair => println("HasSubSequence [sup: %s, sub: %s]: %s".format(
      sListPair._1, sListPair._2, hasSubsequence(sListPair._1, sListPair._2)
    )))
  }
}
