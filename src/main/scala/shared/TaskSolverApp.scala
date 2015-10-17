package shared

import java.io.{FileOutputStream, PrintWriter}
import scala.io.Source
case class TaskSolverApp(taskSolverFunction: Iterator[String] => String) extends App {

  val in = Source.fromFile("./in.txt")
  val out = new PrintWriter(new FileOutputStream("./out.txt"))

  out.print(
    taskSolverFunction(in.getLines())
  )

  out.close()

}
