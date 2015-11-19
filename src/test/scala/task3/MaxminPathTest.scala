package task3

import org.scalatest.{Matchers, FlatSpec}

class MaxminPathTest extends FlatSpec with Matchers {

  "A graph from task definition" should "have correctly constructed maxmin path" in {

    val input =
      """4
        |2 25 3 4 0
        |0
        |2 0 4 7 0
        |0
        |1
        |4
      """.stripMargin.split("\n")

    val (maxminpath, pathWeight) = Solver.solve(input.toIterator)

    maxminpath should be (Seq(1,3,4))
    pathWeight should be (4)

  }

  "A graph with six nodes" should "have correctly constructed maxmin path from 1 to 5" in {
    val input =
      """6
        |2 1 4 5 6 2 0
        |3 2 0
        |3 10 0
        |6 7 3 3 0
        |0
        |5 4 0
        |1
        |5
      """.stripMargin.split("\n")

    val (maxminpath, pathWeight) = Solver.solve(input.toIterator)

    maxminpath should be (Seq(1,4,6,5))
    pathWeight should be (4)
  }

  "A graph with six nodes" should "correctly fail to construct a maxmin path from 6 to 3" in {
    val input =
      """6
        |2 1 4 5 6 2 0
        |3 2 0
        |3 10 0
        |6 7 3 3 0
        |0
        |5 4 0
        |6
        |3
      """.stripMargin.split("\n")

    val (maxminpath, pathWeight) = Solver.solve(input.toIterator)

    maxminpath should be (Seq())
    pathWeight should be (Long.MinValue)
  }

  "A graph with sixteen nodes" should "correctly construct a maxmin path from 1 to 16" in {
    val input =
      """16
        |2 2 4 5 6 7 0
        |5 4 3 5 0
        |5 7 8 12 0
        |6 2 7 10 0
        |7 9 0
        |9 2 11 4 0
        |9 4 10 6 0
        |10 4 13 9 0
        |14 11 12 4 0
        |12 6 13 13 0
        |14 13 0
        |14 12 15 7 0
        |16 16 15 15 0
        |15 14 0
        |16 20 0
        |0
        |1
        |16""".stripMargin.split("\n")

    val (maxminpath, pathWeight) = Solver.solve(input.toIterator)

    maxminpath should (be (Seq(1,4,7,10,13,15,16)) or
      be (Seq(1,4,7,10,12,14,15,16)))
    pathWeight should be (5)
  }

  "A graph with sixteen nodes" should "have correctly constructed maxmin path from 3 to 13" in {
    val input =
      """16
        |2 2 4 5 6 7 0
        |5 4 3 5 0
        |5 7 8 12 0
        |6 2 7 10 0
        |7   9 0
        |9 2 11 4 0
        |9 4 10 6 0
        |10 4 13    9 0
        |14 11 12 4 0
        |12 6 13 13 0
        |14 13 0
        |14 12 15 7 0
        |16 16 15 15 0
        |15 14 0
        |16 20 0
        |0
        |3
        |13""".stripMargin.split("\n")

    val (maxminpath, pathWeight) = Solver.solve(input.toIterator)

    pathWeight should be (9)
    maxminpath should be (Seq(3,8,13))
  }

  "A given graph" should "correctly parse" in {
    val input =
      """9
        |2  2  3 10  4  3  6  7  0
        |3  1  5 10  0
        |5  3  6 10  4 10  0
        |6  2  0
        |7  8  0
        |5  2  7 15  8  5 0
        |5  6  0
        |7  2  0
        |7  1  8  1  0
        |1
        |8
        |
      """.stripMargin.split("\n")

    val (maxminpath, pathWeight) = Solver.solve(input.toIterator)

    pathWeight should be (5)
    maxminpath should be (Seq(1,3,6,8))
  }

}
