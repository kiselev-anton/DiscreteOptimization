package task3

import scala.annotation.tailrec
import scalax.collection.immutable.Graph

import scala.collection.mutable.{PriorityQueue, HashMap}
import scalax.collection.GraphPredef.EdgeLikeIn

/** Путь максимального веса. */
case class ModifiedDijkstra[Node, Edge[X] <: EdgeLikeIn[X]](graph: Graph[Node, Edge]) {

  def distance(v: Node, w: Node) = (graph.get(v) ~> graph.get(w)) // min is here
    .map(_.weight).toSeq.sorted.headOption.getOrElse(Long.MinValue)

  def maxminPath(v: Node, w: Node): (Seq[Node], Long) = {

    val distances = new HashMap[Node, Long]()

    for(node <- graph.nodes.toOuter)
      distances(node) = if(node == v) Long.MaxValue else distance(v, node)

    val previous = new HashMap[Node, Node]()

    implicit def orderingOnNodes = new Ordering[Node] {
      def compare(x: Node, y: Node): Int = distances(y) compare distances(x)
    }

    val queue = PriorityQueue[Node](v)(orderingOnNodes)

    while (queue.nonEmpty) {
      val vertex = queue.dequeue()
      for (neighbor <- graph.get(vertex).diSuccessors.toOuterNodes) {
        val alternative_distance = distances(vertex) min distance(vertex, neighbor)
//        println(vertex, neighbor, alternative_distance)
        if (distances(neighbor) <= alternative_distance) {
          distances(neighbor) = alternative_distance
          previous(neighbor) = vertex
        }
        if (!queue.exists(_ == neighbor)) queue += neighbor
      }
    }

//    println(distances)
//    println(previous)

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
