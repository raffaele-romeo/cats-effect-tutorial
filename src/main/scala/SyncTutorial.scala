import cats.effect._
import cats.effect.syntax._

object SyncTutorial {

  val io1 = printIO("io1 has run!") *> IO.pure("some value").as(40)
  val io2 = printIO("io2 has run!") *> IO.pure(42)

  def runHello: IO[Unit] = for {
    a <- io1
    b <- io2
  } yield (println(a * b))

  val program = io1 *> io2

  val divisionBy0Attempt1 = divide(5, 0).attempt.flatMap {
    case Left(value) => printIO("OK")
    case Right(value) => printIO("Not OK")
  }

  val divisionBy0Attempt2 = divide(5, 0).handleErrorWith{
    case _: java.lang.ArithmeticException => IO.pure(-1)
  }

  val divisionBy0Attempt3 = divide(5, 0).handleError{
    case _: java.lang.ArithmeticException => -1
  }

  val divisionBy0Attempt4 = divide(5, 0).redeemWith(
    error => printIO(s"very bad"),
    result => printIO("All good")
  )

  val bracket = printIO("open").as(25).bracket(n => divide(100, n))(_ => printIO("close"))

  def doIf[A](bool: Boolean, ifTrue: IO[A], ifFalse: IO[A]): IO[A] =
    if(bool) ifTrue else ifFalse

  def threeTimes[A](prog: IO[A]) = prog *> prog *> prog

  def divide(m: Int, n: Int) =
    printIO(s"dividing $m by $n").map(_ => m.doubleValue / n)

  def printIO[A](a: A): IO[Unit] = IO(println(a))
  def printThreadName = printIO(Thread.currentThread().getName)
  val randomInt: IO[Int] = IO(scala.util.Random.nextInt(100))
}
