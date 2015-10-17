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

    val edges = (1 to n).flatMap { vertex =>
      input(vertex-1).split(" ").map(_.toInt).takeWhile(_ != 0).toSeq match {
        case Nil => Seq(vertex ~ vertex)
        case list => list.map(vertex ~ _)
      }
    }

    val graph = Graph.from(edges = edges)

    val components = DepthFirstSearch(graph).components
    components.length should be (4)
  }

  "A Graph from the given test case" should "be correctly counted" in {
    val n = 11

    val input =
      """2 3 0
        |1 0
        |1 0
        |8 0
        |0
        |8 7 0
        |6 0
        |4 6 0
        |10 11 0
        |9 11 0
        |9 10 0
      """.stripMargin.split("\n")

    val edges = (1 to n).flatMap { vertex =>
      val incedentNodes = input(vertex-1).split(" ").map(_.toInt).takeWhile(_ != 0).toSeq
      incedentNodes match {
        case Nil => Seq(vertex ~ vertex)
        case list => list.map(vertex ~ _)
      }
    }

    val graph = Graph.from(edges = edges)

    val components = DepthFirstSearch(graph).components
    components.foreach(println)
    components.length should be (4)
  }

}
