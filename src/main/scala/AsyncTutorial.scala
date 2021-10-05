
import cats.effect.{IO, OutcomeIO, Ref, Resource, ResourceIO, Sync}
import cats.implicits._

import java.time.Instant
import scala.concurrent.duration._

object AsyncTutorial {
  val rts = new RealTimeSync
  val period = 5.seconds

  def program(ts: TimeSync) = {
    val check = ts.getCurrentTimestamp.delayBy(1.seconds)

    (1 to 5).toList.traverse(_ => check)
    //check.foreverM.void
  }

  def set: IO[Set[Int]] =
    Ref.of[IO, Set[Int]](Set.empty)
      .flatMap { ref =>
        (1 to 25).toList
          .parTraverse { i => ref.update(_ + i) }
          .flatMap(_ => ref.get)
      }
}


trait TimeSync {
  def getCurrentTimestamp: IO[Instant]
}

class RealTimeSync extends TimeSync {
  override def getCurrentTimestamp: IO[Instant] = IO(Instant.now)
    .flatTap(ts => SyncTutorial.printIO(s"got $ts from the real server") *> SyncTutorial.printThreadName)
}

class CachedTimeSync private(ref: Ref[IO, Instant]) extends TimeSync {
  override def getCurrentTimestamp: IO[Instant] = ref.get
    .flatTap(ts => SyncTutorial.printIO(s"got $ts from the cache") *> SyncTutorial.printThreadName)
}

object CachedTimeSync {
  def updater(
               cache: Ref[IO, Instant],
               period: FiniteDuration,
               realTimeSync: RealTimeSync): ResourceIO[IO[OutcomeIO[Unit]]] = {
    def loop: IO[Unit] =
      realTimeSync
        .getCurrentTimestamp
        .flatMap(cache.set)
        .delayBy(period) >> loop

    println("In loop")

    loop.background
  }

  def create(realTimeSync: RealTimeSync, period: FiniteDuration) = {
    val cacheF = realTimeSync.getCurrentTimestamp.flatMap(ts => Ref.of[IO, Instant](ts))

    println("in resource")

    Resource.liftK(cacheF).flatMap{ref =>
      updater(ref, period, realTimeSync).map(_ => new CachedTimeSync(ref))
    }
  }
}
