package module1.homework.futures

import module1.homework.futures.HomeworksUtils.TaskSyntax
import zio.CancelableFuture

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    futures.foldRight(Future.successful((List[A](), List[Throwable]()))) {
      (item, acc) =>
        acc.flatMap(a => item.map(i => (i :: a._1, a._2)).recover{
          case ex => (a._1, ex :: a._2)
        })
    }
  }
}
