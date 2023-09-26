package module1.homework

import scala.util.Random

object homework3 {
  class Basket {
    private val basket = List(1, 1, 1, 0, 0, 0)


    def Select(): Boolean = {
      val first = Random.nextInt(6)
      val second = {
        val r = Random.nextInt(5)
        if (r >= first)
          r + 1
        else
          r
      }

      val (min, max) = if (first > second) (second, first) else (first, second)

      basket.view.slice(min, max).zipWithIndex.flatMap {
        case (item, index) => index match {
          case x if x == 0 || x == (max - min) => List(item)
          case _ => Nil
        }
      }

      //      basket.view.foldLeft(Random.nextInt(5), Random.nextInt(4), List[Int])((acc, item) => acc match {
      //        case ()
      //      })
    }.contains(1)
    //      basket.flatMap.foldLeft((Random.nextInt(6), Random.nextInt(5), 0, false))((acc, item) =>
    //      acc match{
    //        case (_, _, _, true) => (0, 0, 0, true)
    //      })
    //List(Random.nextInt(6), Random.nextInt(5)).map(index => )
    //basket.foldLeft(List[Int])((acc, item) => if ).(1)

    /**
     * Хитрожопая, не очень производительная реализация :)
     *
     * @return
     */
    def Select2(): Boolean = Random.shuffle(basket).take(2).contains(1)
  }
}
