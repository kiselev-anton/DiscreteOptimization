package task2

import org.scalatest._
import scalax.collection.immutable.Graph
import scalax.collection.edge.Implicits._
import shared.EdgeExtractor.extractWUnDiEdges

class BoruvkaKruskalAlgorithmTest extends FlatSpec with Matchers {

  "A graph from task definition" should "have correctly constructed MST" in {
    val n = 4

    val input =
      """2 5 3 15 0
        |1 5 3 0 0
        |1 15 2 0 4 25 0
        |3 25 0 """.stripMargin.split("\n")

    val edges = for (vertex <- 0 until n;
                     edge <- extractWUnDiEdges(vertex+1, input(vertex).split(" ").map(_.toInt))) yield edge

    val graph = Graph.from(edges = edges)

    val (mst, mst_weight) = BoruvkaKruskalAlgorithm.solve(graph)

    mst should be (Graph.from(edges = Seq(
      (1 ~% 2)(5),
      (2 ~% 3)(0),
      (3 ~% 4)(25)
    )))

    mst_weight should be (30)
  }

  "A test graph" should "have correctly constructed MST" in {
    val n = 15

    val input =
      """2 1 3 2 11 3 0
        |1 1 3 1 6 4 10 2 0
        |1 2 2 1 4 0 5 4 0
        |3 0 5 5 6 6 0
        |3 4 4 5 7 7 0
        |2 4 4 6 7 2 8 4 10 3 0
        |5 7 6 7 8 3 15 16 0
        |6 4 9 2 15 15 7 3 0
        |10 1 8 2 14 8 13 7 0
        |11 15 12 4 9 1 2 2 6 3 0
        |1 3 12 3 10 15 0
        |11 3 10 4 13 6 0
        |12 6 9 7 14 9 0
        |13 9 15 10 9 8 0
        |7 16 8 15 14 10 0
      """.stripMargin.split("\n")

    val edges = for (vertex <- 0 until n;
                     edge <- extractWUnDiEdges(vertex+1, input(vertex).split(" ").map(_.toInt))) yield edge

    val graph = Graph.from(edges = edges)

    val (mst, mst_weight) = BoruvkaKruskalAlgorithm.solve(graph)

    mst should be (Graph.from(edges = Seq(
      (1 ~% 2)(1),
      (1 ~% 11)(3),
      (2 ~% 3)(1),
      (2 ~% 10)(2),
      (3 ~% 5)(4),
      (3 ~% 4)(0),
      (6 ~% 7)(2),
      (7 ~% 8)(3),
      (9 ~% 8)(2),
      (9 ~% 10)(1),
      (9 ~% 14)(8),
      (11 ~% 12)(3),
      (12 ~% 13)(6),
      (14 ~% 15)(10)
    )))

    mst.edges.toOuter should not contain (8 ~% 15)(15)

    mst_weight should be (46)
  }

}
