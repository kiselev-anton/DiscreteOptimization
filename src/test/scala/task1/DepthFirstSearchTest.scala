package task1

import org.scalatest._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DepthFirstSearchTest extends FlatSpec with Matchers {

  "A Graph with one component" should "be correctly counted" in {
    val n = 4

    val input =
      """2 3 0
        |1 3 0
        |1 2 4 0
        |3 0 """.stripMargin.split("\n")

    val edges = for (vertex <- 0 until n;
                     neighbor <- input(vertex).split(" ").map(_.toInt) if neighbor != 0)
      yield (vertex+1) ~ neighbor

    val graph = Graph.from(edges = edges)

    val components = DepthFirstSearch(graph).components
    components.length should be (1)
  }

  "A Graph with four components" should "be correctly counted" in {
    val n = 12

    val input =
      """2 0
        |1 10 0
        |7 8 0
        |6 0
        |9 0
        |4 0
        |3 8 0
        |3 7 0
        |5 0
        |2 11 12 0
        |10 0
        |10 0 """.stripMargin.split("\n")

    val edges = for (vertex <- 0 until n;
                     neighbor <- input(vertex).split(" ").map(_.toInt) if neighbor != 0)
      yield (vertex+1) ~ neighbor

    val graph = Graph.from(edges = edges)

    val components = DepthFirstSearch(graph).components
    components.length should be (4)
  }

}
