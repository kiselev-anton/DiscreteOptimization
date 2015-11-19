package task4

import scalax.collection.immutable.Graph

object Solver {
  def solve(in: Iterator[String]) = {

    import shared.EdgeExtractor.extractWUnDiEdges

    // input
    val k = in.next().toInt
    val l = in.next().toInt

    val array_size = in.next().toInt

    val incidenceArray = (1 to array_size)
      .flatMap(extractWUnDiEdges(_, in.map(_.toInt).takeWhile(_ != 32767).toSeq))

    val graph = Graph.from(edges = incidenceArray)

    // algorithm
    // TODO алгоритм форда-фалкерсона

    // output
    "" // TODO fix return
  }
}

/**
 *  Найти, если оно есть, полное паpосочетание в двудольном гpафе

     Метод решения: сведение к задаче о максимальном потоке и использование
поиска в глубину для поиска f-дополняющих цепей (М-чеpедующихся).

Файл входных данных:
  Двудольный гpаф G=(X,Y,E), k=|X|, l=|Y|, заданный Х-массивом смежностей.
  X-массив смежностей:  также как и массив смежностей,  только перечисляются
смежные с вершинами x из X.  Для изолиpованной веpшины индексв массиве pавен 0.
  В пеpвой стpоке файла числа k l.  Во втоpой pазмеp массива.  Далее pасположен
массив смежности (  не  более  10  чисел  в  одной  стpоке).
Последний элемент массива pавен 32767.

Файл выходных данных:
     Y и во второй строке полное паросочетание, представленное массивом
XПАРА, или N и во второй строке вершина xi, из которой поиск не удачен.

 */
object Main extends shared.TaskSolverApp(Solver.solve)
