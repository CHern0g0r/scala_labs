import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.concurrent.duration._
import cats.syntax.all._
import cats.effect.concurrent.{MVar, Ref, Semaphore}

object Main extends IOApp{

  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      tick <- mvar.take
      _ <- IO(println(tick))
      _ <- rec
    } yield ()
    Resource.make(rec.start)(_.cancel.flatMap(_ => IO(println("End printing")))).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(cnt: Long): IO[Unit] = for {
      _ <- mvar.put(cnt.toString)
      _ <- IO.sleep(1.seconds)
      _ <- rec(cnt + 1)
    } yield ()
    Resource.make(rec(0).start)(_.cancel.flatMap(_ => IO(println("End counting")))).void
  }

  val exec: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO(println("Stoped")))
    _ <- runCounter(mvar)
    _ <- runPrinter(mvar)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = exec.use(_ => IO.never)
}
