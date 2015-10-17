package task2

import scala.collection.mutable.{HashMap, PriorityQueue}
import scalax.collection.edge.WUnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.edge.Implicits._

/**
 * Поиск минимального остова связного графа. */
object BoruvkaKruskalAlgorithm {


  def MST_edgeSet(graph: Graph[Int, WUnDiEdge]) = {

    type Node = Int
    type Edge = WUnDiEdge[Int]

    val n = graph.nodes.size

    implicit def edgesWeightAscendingOrdering = new Ordering[Edge] {
      override def compare(x: Edge, y: Edge): Int = y.weight compare x.weight
    }

    val queue = PriorityQueue(graph.edges
      .map(e => e.toOuter).toSeq :_*)

    val components = HashMap(graph.nodes.toOuter.zipWithIndex
      .map{ case (node, index) => (index, Set(node)) }.toSeq: _*)

    def merge(component1: Int, component2: Int): Unit = {
      components(component1) ++= components(component2)
      components(component2) = Set.empty
    }

    def component(node: Node) = components.keys.find(components(_).contains(node)).get

    var MST_edges:Set[Edge] = Set.empty
    while (MST_edges.size != n-1) {
      val edge@(v :~ w % weight) = queue.dequeue()

      val component1Index = component(v); val p = components(component1Index)
      val component2Index = component(w); val q = components(component2Index)

      if (component1Index != component2Index) {
        if (p.size > q.size) merge(component1Index, component2Index)
        else                 merge(component2Index, component1Index)
        MST_edges += edge
      }
    }
    MST_edges
  }

  def edges2Weight(edges: Iterable[WUnDiEdge[Int]]) = edges.toSeq.map{case (_ :~ _ % weight) => weight}.sum

  def MST_weight(graph: Graph[Int, WUnDiEdge]) = edges2Weight(MST_edgeSet(graph))

  def MST(graph: Graph[Int, WUnDiEdge]) = Graph.from(edges = MST_edgeSet(graph))

  def solve(graph: Graph[Int, WUnDiEdge]): (Graph[Int, WUnDiEdge], Long) = {
    val edgeSet = MST_edgeSet(graph)
    (Graph.from(edges = edgeSet), edges2Weight(edgeSet))
  }

}
