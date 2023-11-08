package module2.homework

import module2.homework.hw6.Show._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, implicitConversions}

object hw6 {

  def main(args: Array[String]): Unit = {
    import hw6.MonadOps2

    val i = 1
    val l = List(1,2,3)
    
    val o: Option[Option[Int]] = ???

    i.show()
    l.show()

    o.flatten1

    l.map1(x => x)
    l.flatMap1(x => List(x))
  }

  trait Show[A] {
    def show(): Unit
  }

  object Show {
    implicit def fromJvm[A](value: A): Show[A] = Show.fromFunction(value)(_.toString)

    def apply[A](implicit v: Show[A]): Show[A] = v

    def fromFunction[A](a: A)(f: A => String): Show[A] = () => println(f(a))
  }

  trait Monad[F[_]] {
    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

    def pure[A](a: A): F[A]
  }

  implicit class MonadOps[A, F[_]: Monad](fa: F[A]){
    def map1[B](ab: A => B): F[B] = Monad[F].map(fa)(ab)
    def flatMap1[B](afb: A => F[B]): F[B] = Monad[F].flatMap(fa)(afb)
  }

  implicit class MonadOps2[A, F[_]: Monad](ffa: F[F[A]]) {
    def flatten1: F[A] = Monad[F].flatten(ffa)
  }

  object Monad{
    def apply[F[_]](implicit v: Monad[F]): Monad[F] = v

    implicit def from[F[_]](fa: F[_]): Monad[F] = ???

    implicit val optMonad: Monad[Option] = new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

      override def pure[A](a: A): Option[A] = Option(a)
    }

    implicit val listMonad: Monad[List] = new Monad[List] {
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

      override def pure[A](a: A): List[A] = List(a)
    }

    implicit def futMonad(implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
      override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

      override def pure[A](a: A): Future[A] = Future(a)
    }
  }
}