package task3

import scala.annotation.tailrec
import scalax.collection.immutable.Graph

import scala.collection.mutable.{PriorityQueue, HashMap, HashSet}
import scalax.collection.GraphPredef.EdgeLikeIn

/** Путь максимального веса. */
case class ModifiedDijkstra[Node, Edge[X] <: EdgeLikeIn[X]](graph: Graph[Node, Edge]) {

  def distance(v: Node, w: Node) = (graph.get(v) ~> graph.get(w))
    .map(_.weight).toSeq.sorted.headOption.getOrElse(Long.MinValue)

  def maxminPath(v: Node, w: Node): (Seq[Node], Long) = {

    val previous = new HashMap[Node, Node]()
    val distances = new HashMap[Node, Long]()
    val seenNodes = new HashSet[Node]()

    for(node <- graph.nodes.toOuter)
      distances(node) = if (node == v) Long.MaxValue else distance(v, node)

    val queue = PriorityQueue[Node](v)(new Ordering[Node] {
      def compare(x: Node, y: Node): Int = distances(x) compare distances(y)
    })

    while (queue.nonEmpty) {
      val vertex = queue.dequeue()
      seenNodes += vertex
      for (neighbor <- graph.get(vertex).diSuccessors.toOuterNodes) {
        val alternative_distance = distances(vertex) min distance(vertex, neighbor)
        if (distances(neighbor) <= alternative_distance) {
          distances(neighbor) = alternative_distance
          previous(neighbor) = vertex
        }
        if (!queue.exists(_ == neighbor) && !seenNodes(neighbor))
          queue += neighbor
      }
    }

    @tailrec
    def backTraverse(vertex: Node, seq: Seq[Node] = Nil): Seq[Node] = previous.get(vertex) match {
      case Some(next) => backTraverse(next, vertex +: seq)
      case None => if(seq.headOption.exists(node => graph.get(node) isSuccessorOf graph.get(v) )) v +: seq else Nil
    }

    val path = backTraverse(w)
    val pathWeight = if(path.nonEmpty) path.tail.map(node => distances(node)).min else Long.MinValue
    (path, pathWeight)

  }

}
