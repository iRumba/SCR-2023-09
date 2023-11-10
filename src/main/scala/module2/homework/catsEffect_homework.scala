package module2.homework


import cats.effect.IO
import module2.homework.catsEffectHomework.Console.ioConsole.printLine
import module2.homework.catsEffectHomework.Random.ioRandom.nextIntBetween

import scala.language.higherKinds

object catsEffectHomework{

  def main(args: Array[String]): Unit = {
    guessProgram.unsafeRunSync
  }
  /**
   * Тайп класс для генерации псевдо случайных чисел
   * @tparam F
   */
  trait Random[F[_]] {
    /***
     *
     * @param min значение от (включительно)
     * @param max значение до (исключается)
     * @return псевдо случайное число в заданном диапазоне
     */
    def nextIntBetween(min: Int, max: Int): F[Int]
  }



  object Random{
    /**
     * 1. реализовать сумонер метод для класса Random, в последствии он должен позволить
     * использовать Random например вот так для IO:
     * Random[IO].nextIntBetween(1, 10)
     *
     * @return Random[F]
     */
    def apply[F[_]](implicit fa: Random[F]): Random[F] = fa


    /**
     * 2. Реализовать инстанс тайп класса для IO
     */
    implicit val ioRandom: Random[IO] = new Random[IO] {
      /**   *
       *
       * @param min значение от (включительно)
       * @param max значение до (исключается)
       * @return псевдо случайное число в заданном диапазоне
       */
      override def nextIntBetween(min: Int, max: Int): IO[Int] = IO(new util.Random().nextInt(max - min) + min)
    }
  }

  /**
   * Тайп класс для совершения операций с консолью
   * @tparam F
   */
  trait Console[F[_]]{
    def printLine(str: String): F[Unit]
    def readLine(): F[String]
  }

  object Console{
    /**
     * 3. реализовать сумонер метод для класса Console, в последствии он должен позволить
     * использовать Console например вот так для IO:
     * Console[IO].printLine("Hello")
     *
     * @return Console[F]
     */
    def apply[F[_]](implicit fa: Console[F]): Console[F] = fa

    /**
     * 4. Реализовать инстанс тайп класса для IO
     */
    implicit val ioConsole: Console[IO] = new Console[IO] {
      override def printLine(str: String): IO[Unit] = IO(println(str))

      override def readLine(): IO[String] = IO(scala.io.StdIn.readLine())
    }
  }

  /**
   * 5.
   * Используя Random и Console для IO, напишите консольную программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Программа должна выполняться до тех пор, пока пользователь не угадает.
   * Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

    val inputString = for {
      _ <- printLine("Input number between 1 and 3")
      str <- Console[IO].readLine()
    } yield str

  def tryCast(str: String): IO[Either[Throwable, Int]] = IO(str.toInt).attempt

    def predicate[A](cond: A => Boolean): A => IO[Boolean] =
      x => IO(cond(x))

    val wrongStringPredicate: Either[Throwable, Int] => IO[Boolean] =
      predicate[Either[Throwable, Int]] {
        case Left(_) => false
        case Right(_) => true
      }

    val inputWhileNotNumber =
      doWhile4(inputString)(x => tryCast(x)) {
        case Left(_) => false
        case Right(_) => true
      }{ str =>
        printLine(s"$str is not number")
      }.map{
        case Right(b) => b
      }

    val inputWhileNotValid =
      doWhile2(inputWhileNotNumber)(num => num >= 1 && num <= 3)(_ => printLine("Must be between 1 and 3"))

    def inputWhileNotRight(right: Int) =
      doWhile2(inputWhileNotValid)(num => num == right)(num => printLine(s"Fail! It is not $num. Try again"))

    val guessProgram = for {
      n <- nextIntBetween(1, 3)
      _ <- printLine("Try guess number from 1 to 3")
      num <- inputWhileNotRight(n)
      _ <- printLine(s"Yes! It is $num")
    } yield()



  /**
   * 6. реализовать функцию doWhile (общего назначения) для IO, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * Подумайте над сигнатурой, еам нужно принимать эффект и условие относительно его значения, для того чтобы повторять либо заканчивать выполнение.
   */

//  def doWhile[A](io: IO[A])(cond: A => Boolean): IO[A] = for {
//    res <- io
//    rightRes <- if (cond(res)) IO(res) else doWhile(io)(cond)
//  } yield rightRes
//
//  def doWhile2[A](io: IO[A])(cond: A => IO[Boolean]): IO[A] = for {
//    res <- io
//    isRight <- cond(res)
//    rightRes <- if (isRight) IO(res) else doWhile2(io)(cond)
//  } yield rightRes
//
//  def doWhile3[A](io: IO[A])(cond: A => Boolean)(onFalse: A => IO[Any]): IO[A] = for {
//    res <- io
//    rightRes <- if (cond(res)) IO(res) else onFalse(res).flatMap(_ => doWhile3(io)(cond)(onFalse))
//  } yield rightRes

  def doWhile[A](io: IO[A])(cond: A => Boolean): IO[A] =
    doWhile2(io)(cond)(_ => IO())

  def doWhile2[A](io: IO[A])(cond: A => Boolean)(onFalse: A => IO[Any]): IO[A] =
    doWhile3(io)(x => x)(cond)(onFalse)

  def doWhile3[A, B](io: IO[A])(mapping: A => B)(cond: B => Boolean)(onFalse: A => IO[Any]): IO[B] =
    doWhile4(io)(x => IO(mapping(x)))(cond)(onFalse)

  def doWhile4[A, B](io: IO[A])(mapping: A => IO[B])(cond: B => Boolean)(onFalse: A => IO[Any]): IO[B] = for {
    res <- io
    b <- mapping(res)
    rightRes <- if (cond(b)) IO(b) else onFalse(res).flatMap(_ => doWhile4(io)(mapping)(cond)(onFalse))
  } yield rightRes


}

/**
 * 7. Превратите данный объект в исполняемую cats effect программу, которая будет запускать
 * guessProgram
 */
object HomeworkApp{

}