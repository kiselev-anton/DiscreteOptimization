package task2

import scalax.collection.immutable.Graph
import scalax.collection.edge.WUnDiEdge
import scalax.collection.edge.Implicits._

import shared.EdgeExtractor.extractWUnDiEdges

object Main extends shared.TaskSolverApp(in => {

  val n = in.next().toInt
  val edges: Seq[WUnDiEdge[Int]] = (1 to n).flatMap { vertex =>
    val neighbors = in.next().split(" ").collect{case str if str != "" => str.toInt}
    extractWUnDiEdges(vertex, neighbors)
  }

  val graph: Graph[Int, WUnDiEdge] = Graph.from(edges = edges)
  val (mst, mst_weight) = BoruvkaKruskalAlgorithm.solve(graph)

  mst.nodes.toSeq.sortBy(_.toString().toInt).zipWithIndex
    .map{case (node, i) =>
      node.outgoing.toSeq.map(_.toOuter).map{
        case (v :~ w % _) => s"${if(v == i+1) w else v}"
      }.sortBy(_.toInt).mkString(" ")
  }.mkString(" 0\n") + " 0\n" + mst_weight

})
