package shared

import java.io.{FileOutputStream, PrintWriter}
import scala.io.Source

case class TaskSolverApp(taskSolverFunction: Iterator[String] => String)
  extends App {

  val in = Source.fromFile("./in.txt")
  val out = new PrintWriter(new FileOutputStream("./out.txt"))

  out.print(
    taskSolverFunction(in.getLines())
  )

  out.close()

}

object EdgeExtractor {
  import scalax.collection.edge.Implicits._

  def extractWUnDiEdges(vertex: Int, seq: Seq[Int]) = seq.sliding(2,2).collect{
    case neighbor +: weight +: _ => (vertex ~% neighbor)(weight)
  }.toSeq

  def extractWDiEdges(vertex: Int, seq: Seq[Int]) = seq.sliding(2,2).collect{
    case neighbor +: weight +: _ => (vertex ~%> neighbor)(weight)
  }.toSeq

}
