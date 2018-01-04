package mi

sealed trait SOption[+A] {
  def map[B](f: A => B): SOption[B] = {
    ???
  }

  def flatMap[B](f: A => SOption[B]): SOption[B] = {
    ???
  }

  def getOrElse[B >: A](default: => B): SOption[B] = {
    ???
  }

  def orElse[B >: A](ob: => SOption[B]): SOption[B] = {
    ???
  }

  def filter(f: A => Boolean): SOption[A] = {
    ???
  }
}

case class SSome[+A](get: A) extends SOption[A]
case object None extends Option[Nothing]