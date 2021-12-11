import common.loadPackets
import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def neighbors = Set(
    copy(x = x - 1),
    copy(x = x - 1, y = y - 1),
    copy(y = y - 1),
    copy(x = x + 1, y = y - 1),
    copy(x = x + 1),
    copy(x = x + 1, y = y + 1),
    copy(y = y + 1),
    copy(x = x - 1, y = y + 1)
  )
}

type State = Map[Point, Int]

val input = loadPackets(List("day11.txt")).map(_.split("").toList)
val state: State = input.zipWithIndex
  .flatMap { case (row, y) => row.zipWithIndex
    .map { case (depth, x) => Point(x, y) -> depth.toInt }
  }.toMap

@tailrec
def findFlashes(state: State, flashes: Set[Point]): Set[Point] = {
  val next: Set[Point] = flashes ++
    flashes
      .flatMap(_.neighbors)
      .filter(candidate => state
        .get(candidate)
        .exists(_ + candidate.neighbors.intersect(flashes).size > 9))
  if (next == flashes) flashes else findFlashes(state, next)
}

def next(state: State): State = {
  val incremented = state.view.mapValues(_ + 1).toMap
  val flashes = findFlashes(incremented, incremented.filter(_._2 > 9).keySet)
  incremented.map { case (point, value) =>
    (point,
      if (flashes.contains(point)) 0
      else value + point.neighbors.count(flashes.contains))
  }
}

val states: LazyList[State] = LazyList.iterate(state)(next)
val part1 = states.take(101).map(_.values.count(_ == 0)).sum
val part2 = states.indexWhere(_.values.toSet.size == 1)