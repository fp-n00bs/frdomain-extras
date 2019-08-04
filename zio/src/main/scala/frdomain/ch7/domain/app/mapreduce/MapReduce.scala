package frdomain.ch7.domain.app.mapreduce

import zio.{DefaultRuntime, Fiber, Queue, Ref, UIO, ZIO}
import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._
import zio.blocking._

object MapReduce  {

  def readContents(path: Path): ZIO[Blocking, Throwable, String] =
    effectBlocking {
      new String(
        Files.readAllBytes(path),
        "UTF-8"
      )
    }
    private def createMapWorker[A](inputQueue: Queue[Path], reduceQueue: Queue[A])(map: String => A) =
      (for {
        path <- inputQueue.take
        contents <- readContents(path)
        a = map(contents)
        _ <- reduceQueue.offer(a)
      } yield ()).forever.fork

  private def createReduceWorker[A, B](inputQueue: Queue[A], outputQueue: Queue[B], latest: Ref[B])
                                (reduce: (B, A) => B) =
      (for {
        b <- latest.get
        a <- inputQueue.take
        newB = reduce(b, a)
        _ <- latest.set(newB)
        _ <- outputQueue.offer(newB)
      } yield ()).forever.fork

    def mapReduce[A, B](inputQueue: Queue[Path], reduceQueue: Queue[A], outputQueue: Queue[B], workers: Int)
                       (map: String => A)
                       (z: B)(reduce: (B, A) => B): ZIO[Blocking, Throwable, Unit] = {
      val mapWorkers = List.fill(workers)(createMapWorker(inputQueue, reduceQueue)(map))
      val reduceWorker =
        for {
          bRef <- Ref.make(z)
          fiber <- createReduceWorker(reduceQueue, outputQueue, bRef)(reduce)
        } yield fiber

      for {
        mapFibers <- ZIO.collectAll(mapWorkers)
        reduceFiber <- reduceWorker
        _ <- Fiber.joinAll(reduceFiber :: mapFibers)
      } yield ()
    }
}
