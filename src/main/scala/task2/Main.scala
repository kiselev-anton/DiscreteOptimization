package task2

import scalax.collection.immutable.Graph
import scalax.collection.edge.WUnDiEdge
import scalax.collection.edge.Implicits._

object Main extends shared.TaskSolverApp(in => {

  def extractEdgesForSeq(vertex: Int, seq: Seq[Int]) = seq.sliding(2,2).collect{
    case neighbor +: weight +: _ => (vertex ~% neighbor)(weight)
  }.toSeq

  val n = in.next().toInt
  val edges: Seq[WUnDiEdge[Int]] = (1 to n).flatMap { vertex =>
    val neighbors = in.next().split(" ").collect{case str if str != "" => str.toInt}
    extractEdgesForSeq(vertex, neighbors)
  }

  val graph: Graph[Int, WUnDiEdge] = Graph.from(edges = edges)
  val (mst, mst_weight) = BoruvkaKruskalAlgorithm.solve(graph)

  mst.nodes.size + "\n" +
    mst.nodes.toSeq.sortBy(_.toString().toInt).zipWithIndex
    .map{case (node, i) =>
      node.outgoing.toSeq.map(_.toOuter).map{
        case (v :~ w % _) => s"${if(v == i+1) w else v}"
      }.sortBy(_.toInt).mkString(" ")
  }.mkString(" 0\n") + " 0\n" + mst_weight
})
