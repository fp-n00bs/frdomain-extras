package frdomain.ch7.domain.app.MapReduce

import zio.{DefaultRuntime, Fiber, Queue, Ref, UIO, ZIO}
import java.nio.file.{Files, Paths, Path}
import scala.collection.JavaConverters._
import zio.blocking._

object MapReduce extends App with DefaultRuntime {

  unsafeRun(countAllWords("C:/Users/Dan/Desktop/wordCount", 5))

    def listFiles(dir: String): ZIO[Blocking, Throwable, List[Path]] =
      effectBlocking {
        Files.list(Paths.get(dir)).iterator.asScala.toList
      }

    def readContents(path: Path): ZIO[Blocking, Throwable, String] =
      effectBlocking {
        new String(
          Files.readAllBytes(path),
          "UTF-8"
        )
      }

    def mapReduce[A, B](dir: String)
                       (map: String => A)
                       (z: B)(reduce: (B, A) => B): ZIO[Blocking, Throwable, B] =
      for {
        paths <- listFiles(dir)
        mapped <- ZIO.foreach(paths) { path =>
          readContents(path).map { contents =>
            map(contents)
          }
        }
      } yield mapped.foldLeft(z)(reduce)

    def createMapWorker[A](inputQueue: Queue[Path], reduceQueue: Queue[A])(map: String => A) =
      (for {
        path <- inputQueue.take
        contents <- readContents(path)
        a = map(contents)
        _ <- reduceQueue.offer(a)
      } yield ()).forever.fork

    def createReduceWorker[A, B](inputQueue: Queue[A], outputQueue: Queue[B], latest: Ref[B])
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

    def printer(queue: Queue[Long]) =
      (for {
        count <- queue.take
        _ <- UIO(println("Current count:" + count))
      } yield ()).forever.fork

    def countAllWords(dir: String, workers: Int) =
      for {
        inputQueue <- Queue.bounded[Path](16)
        reduceQueue <- Queue.bounded[Long](16)
        outputQueue <- Queue.bounded[Long](16)
        paths <- listFiles(dir)
        _ <- inputQueue.offerAll(paths)
        _ <- mapReduce(inputQueue, reduceQueue, outputQueue, workers)(
          _.split(' ').size)(0L)(_ + _).fork
        _ <- printer(outputQueue)
      } yield ()


}
