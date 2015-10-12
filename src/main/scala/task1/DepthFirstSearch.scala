package task1

import scala.annotation.tailrec
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

case class DepthFirstSearch(graph: Graph[Int, UnDiEdge]) {

  type NodeT = Graph[Int, UnDiEdge]#NodeT

  private case class GraphTraverse(
        stack: Seq[NodeT] = Nil,
        component: Seq[NodeT] = Nil,
        components: Seq[Seq[NodeT]] = Nil)
  {
    lazy val unseenNode = unseenNodes.headOption
    lazy val seenNodes: Set[NodeT] = components.flatten.toSet ++ component
    lazy val unseenNodes: Set[NodeT] = graph.nodes.toSet -- seenNodes
    lazy val backtrack = copy(stack = stack.tail)
    lazy val createConnectedComponent = copy(
      stack = Nil, component = Nil,
      components = component +: components)

    def neighbors(node: NodeT) = node.neighbors.filter(!seenNodes.contains(_))
    def addSeenNode(node: NodeT) = copy(
      stack = node +: stack,
      component = node +: component
    )
  }

  @tailrec
  private final def search(traverse: GraphTraverse = GraphTraverse()): GraphTraverse = {

    traverse.stack.headOption match {
      case Some(node) => traverse.neighbors(node).headOption match {
        case Some(neighbor) => search(traverse.addSeenNode(neighbor))
        case None =>
          if (traverse.stack.isDefined)
            search(traverse.backtrack)
          else search(traverse.createConnectedComponent)
      }
      case None => traverse.unseenNode match {
        case Some(node) => if(traverse.component.nonEmpty) search(traverse.createConnectedComponent.addSeenNode(node))
                           else search(traverse.addSeenNode(node))
        case None => traverse.createConnectedComponent
      }
    }
  }

  def components = search().components

}
