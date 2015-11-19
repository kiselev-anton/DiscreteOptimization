package task3

import scalax.collection.edge.WDiEdge
import scalax.collection.immutable.Graph
import shared.EdgeExtractor.extractWDiEdges

object Solver {
  def solve(in: Iterator[String]) = {

    val n = in.next().toInt

    val edges: Seq[WDiEdge[Int]] = 1 to n flatMap { vertex =>
      val neighbors = in.next().split("\\ +").collect{case str if str != "" => str.toInt}
      extractWDiEdges(vertex, neighbors)
    }

    val graph: Graph[Int, WDiEdge] = Graph.from(edges = edges)

    val v = in.next().toInt
    val w = in.next().toInt

    ModifiedDijkstra(graph).maxminPath(v, w)

  }

  def solvePrint(in: Iterator[String]) = {
    val (maxminpath, pathWeight) = solve(in)
    if(maxminpath.isEmpty) "N"
    else "Y\n" + maxminpath.mkString(" ") + "\n" + pathWeight
  }
}

object Main extends shared.TaskSolverApp(Solver.solvePrint)
