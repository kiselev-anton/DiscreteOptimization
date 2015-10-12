package task2

import scala.io.StdIn._
import scalax.collection.immutable.Graph
import scalax.collection.edge.WUnDiEdge
import scalax.collection.edge.Implicits._

object Main extends App {

  def extractEdgesForSeq(vertex: Int, seq: Seq[Int]) = seq.sliding(2,2).map{
    case Vector(neighbor, weight) => (vertex ~% neighbor)(weight)
  }.toSeq

  val n = readInt()

  val edges: Seq[WUnDiEdge[Int]] = (0 until n).flatMap { vertex =>
    val neighbors = readLine().split(" ").map(_.toInt)
    extractEdgesForSeq(vertex, neighbors)
  }

  val graph: Graph[Int, WUnDiEdge] = Graph.from(edges = edges)

  val MST = BoruvkaKruskalAlgorithm.MST(graph)

}
