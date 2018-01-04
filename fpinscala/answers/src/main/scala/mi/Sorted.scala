package mi


object Sorted {
	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		@annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) {
        true
      } else if (!ordered(as(n), as(n+1))) {
				false
			} else {
				loop(n + 1)
			}
		}

		loop(0)
	}

	def main(args: Array[String]): Unit = {

    val desc = (a: Int, b: Int) => a <= b
		println((Array.empty.mkString(","), isSorted(Array.empty, desc)))
		println((Array(1).mkString(","), isSorted(Array(1), desc)))
		println((Array(1, 2, 2).mkString(","), isSorted(Array(1, 2, 2), desc)))
		println((Array(1, 3, 2).mkString(","), isSorted(Array(1, 3, 2), desc)))
	}	
}
