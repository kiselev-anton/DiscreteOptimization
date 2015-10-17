package task2

import java.io.{FileOutputStream, PrintWriter, FileInputStream}
import java.util.Scanner
import scalax.collection.immutable.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import scalax.collection.edge.Implicits._

object Main extends App {

  def extractEdgesForSeq(vertex: Int, seq: Seq[Int]) = seq.sliding(2,2).collect{
    case neighbor +: weight +: _ => (vertex ~% neighbor)(weight)
  }.toSeq

  val in = new Scanner(new FileInputStream("./in.txt"))
  val out = new PrintWriter(new FileOutputStream("./out.txt"))

  val n = in.nextInt()
  val edges: Seq[WUnDiEdge[Int]] = (1 to n).flatMap { vertex =>
    val neighbors = in.nextLine().split(" ").collect{case str if str != "" => str.toInt}
    extractEdgesForSeq(vertex, neighbors)
  }

  val graph: Graph[Int, WUnDiEdge] = Graph.from(edges = edges)
  val MST = BoruvkaKruskalAlgorithm.MST(graph)

  out.println(MST.nodes.size)
  out.println(MST.nodes
    .map(_.outgoing.toSeq.zipWithIndex
      .map{case (edge: WDiEdge[Int], i: Int) => s"${edge.last} ${edge.weight}" }
      .mkString(" "))
    .mkString(" 0\n") + " 0")

}
