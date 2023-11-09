package module2.homework

import module2.homework.utils.printEffectRunningTime
import sun.util.calendar.BaseCalendar.Date
import zio.{Has, Task, UIO, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, currentTime, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.time.LocalDate
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

object R{
  def main(args: Array[String]): Unit = {
    //zio.Runtime.default.unsafeRun(zio_homework.guessProgram)
    //zio.Runtime.default.unsafeRun(zio_homework.app.exitCode)
    zio.Runtime.default.unsafeRun(zio_homework.appSpeedUp.exitCode)
  }
}

package object utils {
  def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]) = for {
    startTime <- currentTime(TimeUnit.SECONDS)
    _ <- effect
    endTime <- currentTime(TimeUnit.SECONDS)
    _ <- putStrLn(s"Elapsed ${endTime - startTime} seconds")
  } yield ()
}
package object zio_homework {


  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  val inputString = for {
    _ <- putStrLn("Input number between 1 and 3")
    n <- getStrLn
  } yield n

  val inputNumOrNothing = for {
    str <- inputString
    num <- tryCast(str)//ZIO.effect(str.toInt)
  } yield num

  def cast(str: String) = for {
    num <- ZIO.effect(str.toInt)
  } yield num

  def tryCast(str: String): Task[Either[Unit, Int]] = for {
    num <- cast(str).fold(
      _ => Left(),
      data => Right(data)
    )
  } yield num

  val inputWhileNotNumber = for {
    eNum <- doWhile(inputNumOrNothing)({
      case Left(a) => false
      case Right(b) => true
    })
    num <- eNum match {
      case Right(b) => ZIO.succeed(b)
    }
  } yield num

  val inputWhileNotValid = for {
    num <- doWhile(inputWhileNotNumber)(r => r >= 1 && r <= 3)
  } yield num

  lazy val guessProgram = for {
    _ <- setSeed(System.currentTimeMillis())
    rand <- nextIntBetween(1, 3)
    n <- inputWhileNotValid
    res <- if (n == rand) ZIO.succeed("Right") else ZIO.succeed(s"Wrong (expected $rand)")
    _ <- putStrLn(res)
  } yield()



  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(cond: A => Boolean): ZIO[R, E, A] = for {
    res <- effect
    rightRes <- if (cond(res)) ZIO.succeed(res) else doWhile(effect)(cond)
  } yield rightRes


  /**
   * 3. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 3.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = for {
    _ <- ZIO.sleep(1 second)
    num <- nextIntBetween(0, 10)
  } yield num

  /**
   * 3.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  
  /**
   * 3.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

    def effectsWorker[R, A](effects: List[URIO[R, A]])(work: List[A] => Unit) = for {
      res <- effects.foldLeft(ZIO.fromFunction[R, List[A]](_ => List[A]()))((acc, item) => for {
        i <- item
        a <- acc
      } yield i :: a)
      _ <-  ZIO.effect(work(res)).orDie
    } yield ()

    val work = for {
      _ <- effectsWorker(effects)(s => println(s"Sum: ${s.sum}"))
    } yield()

  lazy val app = for{
    _ <- printEffectRunningTime(work)
  } yield ()


  /**
   * 3.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  def effectsSpeedWorker[R, A](effects: List[URIO[R, A]])(work: List[A] => Unit) = for {
    list <- ZIO.collectAllPar(effects)
    _ <- ZIO.effect(work(list))
//    res <- effects.foldLeft(ZIO.fromFunction[R, List[A]](_ => List[A]()))((acc, item) => for {
//      i <- item
//      a <- acc
//    } yield i :: a)
//    _ <- ZIO.effect(work(res)).orDie
  } yield ()

  val speedWork = for {
    _ <- effectsSpeedWorker(effects)(s => println(s"Sum: ${s.sum}"))
  } yield ()

  lazy val appSpeedUp = for {
    _ <- printEffectRunningTime(speedWork)
  } yield ()


  /**
   * 4. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */


   /**
     * 5.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = ???

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appSpeedUp.exitCode

}
