import module1.{hof, type_system, opt, list}

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello, World!")

    val r: String => Unit = hof.logRunningTime(hof.doomy)
    r("Doomy")


    println("Option homework")
    val ov1 = opt.Option.fromValue(5)
    val ov2 = opt.Option.fromValue(3)
    var oe1 = opt.Option.empty[Int]();

    ov1.printIfAny()
    oe1.printIfAny()

    println(ov1.zip(ov2))
    println(ov1.zip(oe1))

    println(ov1.filter(x => x == 5))
    println(ov1.filter(x => x == 3))
    println(oe1.filter(x => x == 3))

    println();
    println("List homework");

    val l1: list.List[Int] = list.List(1, 2, 3)

    println("cons")
    println(l1.cons(0).mkString(", "))

    println("mkString")
    println(l1.mkString(", "))

    println("reverse")
    println(l1.reverse().mkString(", "))

    println("map")
    println(l1.map(x => s"($x)").mkString(", "))

    println("filter")
    println(l1.filter(x => x != 2).mkString(", "))

    val l2 = list.List(new A)

    val l3 = l2.cons(new A)

    val l4 = l3.cons(new B)

    val l5 = l4.cons(new A)
    println("cons types")
    println(l5.mkString(", "))

    val l6 = l1.cons(5.5)
    val l7 = l6.cons(5)

    val l8 = list.List(new B)
    val l9 = l8.cons(new A)
  }
}

class A {
  override def toString() = {
    "A"
  }
}

class B extends A {
  override def toString() = {
    "B"
  }
}