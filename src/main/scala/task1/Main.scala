package task1

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

object Main extends shared.TaskSolverApp(in => {
  val n = in.next().toInt
  val edges = (1 to n).flatMap { vertex =>
    in.next().split(" ").map(_.toInt).takeWhile(_ != 0).toSeq match {
      case Nil => Seq(vertex ~ vertex)
      case list => list.map(vertex ~ _)
    }
  }

  val graph = Graph.from(edges = edges)
  val components = DepthFirstSearch(graph).components

  components.length + "\n" +
  components.map(
    _.map(_.toString().toInt).sorted).sortBy(_.head)
    .map(_.mkString(" ") + " 0").mkString("\n")
})
