import cats.effect.{IO, IOApp, Sync}

import scala.concurrent.duration._
import cats.effect.unsafe.implicits.global


object Main extends IOApp.Simple {
  override def run: IO[Unit] = {
    SyncTutorial.printThreadName.unsafeRunSync()
    CachedTimeSync.create(AsyncTutorial.rts, AsyncTutorial.period).use { ts =>
      AsyncTutorial.program(ts).void
    }.unsafeRunSync()

    SyncTutorial.printThreadName
  }
}