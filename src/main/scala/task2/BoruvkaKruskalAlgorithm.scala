package task2

import scala.collection.mutable.{HashMap, PriorityQueue}
import scalax.collection.edge.WUnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.edge.Implicits._

/**
 * Поиск минимального остова связного графа. */
object BoruvkaKruskalAlgorithm {


  def MST_edgeSet(graph: Graph[Int, WUnDiEdge]) = {

    type Node = graph.NodeT
    type Edge = WUnDiEdge[Node]

    val n = graph.nodes.size

    implicit def edgesWeightAscendingOrdering = new Ordering[Edge] {
      override def compare(x: Edge, y: Edge): Int = y.weight compare x.weight
    }

    val queue = PriorityQueue(graph.edges
      .map(e => (e.head ~% e.tail.head)(e.weight)).toSeq :_*)

    val components = HashMap(graph.nodes.zipWithIndex
      .map{ case (edge, index) => (index, Set(edge)) }.toSeq: _*)

    def size(component: Int) = components(component).size

    def merge(component1: Int, component2: Int): Unit = {
      components(component1) ++= components(component2)
      components(component2) = Set.empty
    }

    def component(node: Node) = components.keys.find(components(_).contains(node)).get

    var MST_edges:Set[WUnDiEdge[Int]] = Set.empty
    while (MST_edges.size != n-1) {
      val edge@(v :~ w % weight) = queue.dequeue()

      val component1Index = component(v)
      val component2Index = component(w)
      val p = components(component1Index); val q = components(component2Index)

      if (component1Index != component2Index) {
        if (p.size > q.size) merge(component1Index, component2Index)
        else                 merge(component2Index, component1Index)
        MST_edges += (v.toInt ~% w.toInt)(weight)
      }
    }
    MST_edges
  }


  def MST(graph: Graph[Int, WUnDiEdge]) = Graph.from(edges = MST_edgeSet(graph))

}
