package frdomain.ch7.domain.app.mapreduce

import zio.{DefaultRuntime, Fiber, Queue, Ref, UIO, ZIO}
import zio.blocking._

object MapReduce  {

  def mapReduce[A, B, C, D](inputSource: List[A], inputQueue: Queue[A], reduceQueue: Queue[B], outputQueue: Queue[C], workers: Int)
                           (sourceMapper: A => D)
                           (reduceMapper: D => B)
                           (z: C)(reduce: (C, B) => C): ZIO[Blocking, Throwable, Unit] = {
    val mapWorkers = List.fill(workers)(createMapWorker(inputSource, inputQueue, reduceQueue) (sourceMapper)(reduceMapper))
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

    private def createMapWorker[A, B, D](inputSource: List[A], inputQueue: Queue[A], reduceQueue: Queue[B])
                                        (sourceMapper: A => D)
                                        (reduceMapper: D => B) =
      (for {
        _      <- inputQueue.offerAll(inputSource)
        source <- inputQueue.take
        content = sourceMapper.apply(source)
        a = reduceMapper(content)
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
}
