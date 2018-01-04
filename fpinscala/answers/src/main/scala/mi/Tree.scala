package mi

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = {
    def go(tree: Tree[A]): Int = {

      tree match {
        case Leaf(_) => 1
        case Branch(l, r) =>
          1 + go(l) + go(r)
      }
    }

    go(tree)
  }

  def max(tree: Tree[Int]): Int = {
    def go(tree: Tree[Int]): Int = {

      tree match {
        case Leaf(v) => v
        case Branch(l, r) =>
          Math.max(go(l), go(r))
      }
    }

    go(tree)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(tree: Tree[A], d: Int): Int = {

      tree match {
        case Leaf(_) => d + 1
        case Branch(l, r) =>
          math.max(go(l, d + 1), go(r, d + 1))
      }
    }

    go(tree, 0)
  }

  def foldRight[A, B](as: SList[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = {
    def go(tree: Tree[A]): Tree[B] = {
      tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) =>
          Branch(go(l), go(r))
      }
    }

    go(tree)
  }

  def main(args: Array[String]): Unit = {
    println(
      "size of (1): %d".format(
        size(Leaf(1)))
    )

    println(
      "size of (1, 2): %d".format(
        size(Branch(Leaf(1), Leaf(2)))
      )
    )

    println(
      "max of (1, (2, 3): %d".format(
        max(Branch(Leaf(1), Branch(Leaf(23), Leaf(3))))
      )
    )

    val tree1 = Branch(
      Leaf(1),
      Branch(
        Leaf(2),
        Branch(
          Leaf(3),
          Branch(
            Leaf(4),
            Leaf(5)
          )
        )
      )
    )

    println(
      "depth of tree1(1, (2, (3, (4, 5)))): %d".format(
        depth(tree1)
      )
    )

    println(
      "map: Power of tree1: %s".format(
        map(tree1, (v: Int) => math.pow(v, v))
      )
    )
  }
}