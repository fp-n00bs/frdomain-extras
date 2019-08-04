package frdomain.ch7.domain.app.mapreduce

import java.nio.file.{Files, Path, Paths}


import zio.blocking._
import zio._

import scala.collection.JavaConverters._

object WordCount extends DefaultRuntime {

  def main(args: Array[String]): Unit = {
    val path = "C:/Users/Dan/IdeaProjects/frdomain-extras-noobs/zio/src/main/resources"
    unsafeRun(countAllWords(path, 5))
    Thread.sleep(10000)
  }

  def countAllWords(dir: String, workers: Int) =
    for {
      inputQueue  <- Queue.bounded[Path](16)
      reduceQueue <- Queue.bounded[Long](16)
      outputQueue <- Queue.bounded[Long](16)
      files       <- listFiles(dir)
      _           <- MapReduce.mapReduce(files, inputQueue, reduceQueue, outputQueue, workers)(readContents(_))(_.split(' ').size)(0L)(_ + _).fork
      _           <- printer(outputQueue)
    } yield ()

  def listFiles(dir: String): ZIO[Blocking, Throwable, List[Path]] =
    effectBlocking {
      Files.list(Paths.get(dir)).iterator.asScala.toList //DPDPDP lagg till bracket
    }

  def readContents(path: Path): String =
    new String(
      Files.readAllBytes(path), //DPDPDP lagg till bracket
      "UTF-8") //DPDPDP fiza sa att EffactivBlocking anvands


  def printer(queue: Queue[Long]) =
    (for {
      count <- queue.take
      _ <- UIO(println("Current count: " + count))
    } yield ()).forever.fork

}
