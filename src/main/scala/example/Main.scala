package zio

sealed trait ZIO[+A] { self =>
  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
    ZIO.Zip(self, that)

  def map[B](f: A => B): ZIO[B] =
    ZIO.Map(self, f)

  def flatMap[B](f: A => ZIO[B]): ZIO[B] =
    ZIO.FlatMap(self, f)

  def run(callback: A => Unit): Unit
}

object ZIO {

  def succeed[A](value: => A): ZIO[A] =
    ZIO.Effect(() => value)

  def succeedNow[A](value: A): ZIO[A] = Succeed(value)

  case class Succeed[A](value: A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(value)
  }

  case class Effect[A](f: () => A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(f())
  }

  case class Zip[A, B](left: ZIO[A], right: ZIO[B]) extends ZIO[(A, B)] {
    override def run(callback: ((A, B)) => Unit): Unit =
      left.run { a =>
        right.run { b =>
          callback(a, b)
        }
      }
  }

  case class Map[A, B](zio: ZIO[A], f: A => B) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      zio.run { a =>
        callback(f(a))
      }
  }

  case class FlatMap[A, B](zio: ZIO[A], f: A => ZIO[B]) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      zio.run { a =>
        // f(a).run { b =>
        //   callback(b)
        // }
        f(a).run(callback)
      }
  }
}

// example scenerios below that are runnable
//

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

object succeed extends ZIOApp[Unit] {
  val howdyZIO = ZIO.succeed(println("howdy!"))
  val howdyZIO2 = ZIO.succeed(println("howdy!"))
  def run: ZIO[Unit] = howdyZIO
}

object zip extends ZIOApp[(Int, String)] {
  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(8) zip ZIO.succeed("LO")

  def run: ZIO[(Int, String)] = zippedZIO
}

object map extends ZIOApp[Unit] {
  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(8) zip ZIO.succeed("LO")

  val personZIO: ZIO[Person] =
    zippedZIO.map { case (int, string) =>
      Person(string, int)
    }

  val mappedZIO: ZIO[String] =
    zippedZIO.map { case (int, string) =>
      string * int
    }

  def run: ZIO[String] = mappedZIO
}

object mapUhOh extends ZIOApp[Unit] {
  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(8) zip ZIO.succeed("LO")

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val mappedZIO: ZIO[ZIO[Unit]] =
    zippedZIO.map { tuple =>
      printLine(s"My tuple is: $tuple")
    }

  def run: ZIO[ZIO[Unit]] = mappedZIO
}

object flatMap extends ZIOApp[Unit] {
  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(8) zip ZIO.succeed("LO")

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val flatMappedZIO: ZIO[Unit] =
    zippedZIO.flatMap { tuple =>
      printLine(s"My tuple is: $tuple")
    }

  def run: ZIO[Unit] = flatMappedZIO
}

object forComprehension extends ZIOApp[Unit] {
  val zippedZIO: ZIO[(Int, String)] =
    ZIO.succeed(8) zip ZIO.succeed("LO")

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val forComprehensionZIO: ZIO[String] =
    for {
      tuple <- zippedZIO
      _ <- printLine(s"My BEAUTIFUL tuple using forComp: $tuple")
    } yield "Nice"

  def run: ZIO[String] = forComprehensionZIO
}
