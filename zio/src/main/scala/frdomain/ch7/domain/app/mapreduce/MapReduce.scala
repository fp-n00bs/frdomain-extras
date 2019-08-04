package frdomain.ch7.domain.app.mapreduce

import zio.{DefaultRuntime, Fiber, Queue, Ref, UIO, ZIO}
import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._
import zio.blocking._

object MapReduce  {

    private def createMapWorker[A, B, D](inputQueue: Queue[A], reduceQueue: Queue[B], contents :(A) => D )(map: D => B) =
      (for {
        source <- inputQueue.take
        content = contents.apply(source)
        a = map(content)
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

    def mapReduce[A, B, C, D](inputQueue: Queue[A], reduceQueue: Queue[B], outputQueue: Queue[C], contents :(A) => D, workers: Int)
                       (map: D => B)
                       (z: C)(reduce: (C, B) => C): ZIO[Blocking, Throwable, Unit] = {
      val mapWorkers = List.fill(workers)(createMapWorker(inputQueue, reduceQueue, contents :(A) => D)(map))
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
