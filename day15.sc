import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day15.txt"))

case class Point(x: Int, y: Int) {
  def neighbors = List(
    copy(x = x - 1),
    copy(x = x + 1),
    copy(y = y - 1),
    copy(y = y + 1)
  )
}

val risk: Map[Point, Int] = input.zipWithIndex.flatMap { case (row, y) =>
  row.zipWithIndex.map { case (risk, x) => Point(x, y) -> risk.toString.toInt }
}.toMap

case class Path(visited: List[Point] = List(Point(0, 0)), totalRisk: Int = 0) {
  def neighbors: List[Path] = visited.head.neighbors
    .filter(risk.keySet.contains)
    .map(n => Path((n :: visited), totalRisk + risk(n)))
}

@tailrec
def bfs(queue: List[Path], minimalRisk: Map[Point,Int]): Map[Point,Int] = queue match {
  case Nil => minimalRisk
  case current :: rest =>
    if (minimalRisk.get(current.visited.head).exists(_ <= current.totalRisk)) {
      bfs(rest, minimalRisk)
    } else {
      bfs(rest ::: current.neighbors,
        minimalRisk.updated(current.visited.head, current.totalRisk))
    }
}

val risks = bfs(List(Path()), Map())
val end = Point(input.head.length - 1, input.length - 1)
risks(end)
