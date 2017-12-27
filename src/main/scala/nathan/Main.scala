package nathan

import nathan.tree._

import scala.collection.mutable.ListBuffer

object Main extends App {
  val t = Branch(
    1,
    List(
      Branch(2,
        List(
          Leaf(7),
          Leaf(8))),
      Leaf(3),
      Branch(4, List(
        Leaf(5),
        Leaf(6)))))
  t.foreach(println)
  println("******" * 10)
  println(t.toList)
}