package zio

sealed trait ZIO[+A] {
  def run(callback: A => Unit): Unit
}

object ZIO {
  def succeedNow[A](value: A): ZIO[A] = Succeed(value)

  case class Succeed[A](value: A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(value)
  }
}

case class Person(name: String, age: Int)
object Person {
  val peter: Person = Person("Peter", 40)
}

trait ZIOApp[A] {
  def run: ZIO[Any]

  def main(args: Array[String]): Unit =
    run.run { result =>
      println(s"The result was $result")
    }
}
object succeedNow extends ZIOApp[Person] {
  val peterZIO: ZIO[Person] =
    ZIO.succeedNow(Person.peter)

  def run: ZIO[Person] = peterZIO
}
