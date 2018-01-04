package mi


object MyModule {
  private def abs(n: Int): Int = {
    if (n < 0) {
      -n
    } else {
      n
    }
  }

  private def messageAbs(n: Int): String = {
    val msg = "The absolute value of %d is %d."
    msg.format(n, abs(n))
  }

  def main(args: Array[String]): Unit = {
    println(messageAbs(-42))
  }
}

//+ Lambda serialization only works with Java serialization
//+ serialization may be a problem with classes; place methods in Objects
//+ scala 11 compiles objects to special classes with $. scala 12 uses Java8 lambdas
//+ promises and futures
//+ there is no master in Scio. Everything is parallel (prep, input, processing, output)
//+ map serializes the func per element in collection
