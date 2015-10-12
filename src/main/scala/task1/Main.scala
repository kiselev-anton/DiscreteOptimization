package task1

import scala.io.StdIn._
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

object Main extends App {

  val n = readInt()

  val edges = for (vertex <- 0 until n;
                   neighbor <- readLine().split(" ").map(_.toInt) if neighbor != 0)
    yield (vertex+1) ~ neighbor

  val graph = Graph.from(edges = edges)

  val components = DepthFirstSearch(graph).components

  println(components.length)
  components.map(_.map(_.toString().toInt).sorted).sortBy(_.head)
    .foreach(component => println(component.mkString(" ") + " 0"))

}
