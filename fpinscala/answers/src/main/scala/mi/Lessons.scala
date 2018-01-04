package mi


case class SomeError(s: String) extends Error


object Lessons {

  def main(args: Array[String]): Unit = {

    val x1: Either[Error, String] = Right("Yellow")
    val x2: Either[Error, Int] = Left(SomeError("Wrong!"))

    val x3 = for {
      y1 <- x1
      y2 <- x2
    } yield (y1, y2)

    print(x3)

    //There is no driver on DataFlow, that's why we can't do iteration
    //Better to read avro data to case classes
    //Check out BCD way of defining case classes and validating data
    //Vectors are better than Arrays for reasoning about behaviour (don't change)
    //But Arrays are more efficient
    //Dont' test encryption/decryption with business logic; Abstract things
    //Minor logic in business functions? No
    //Use flatMap instead of filter
    //Return either[failed, worked]
    //Check BCD tests (checkAll) => scalaCheck + Ratatouli
    //Use case classes, not Avro files
    //Don't filter out nulls. Don't use nulls (use types [case classes], pattern matching, flatMap, types
    //map -> filter is bad
    //cogroup instead of join
    //don't use map with cogroup because they are not lazy
    //use iterator.view.map
    //same for groupBy
    //how to handle duplicate IDs: drop(1).nonEmpty or headOption or
    //use for comprehension (put it a separate function)

    List.empty


  }
}