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

type StateWithFlashcount = (Int, State)

def next(stateWithFlashcount: StateWithFlashcount): StateWithFlashcount = stateWithFlashcount match {
  case (flashCount, state) =>
    val incremented = state.view.mapValues(_ + 1).toMap
    val flashes = findFlashes(incremented, incremented.filter(_._2 > 9).keySet)
    (flashCount + flashes.size,
      incremented.map { case (point, value) => (point,
        if (flashes.contains(point)) 0
        else value + point.neighbors.count(flashes.contains)) })
}

def printState(state: State): String =
  input.indices.map(y => input.head.indices
    .map(Point(_, y)).map(state(_)).mkString).mkString("\n")

val initial: StateWithFlashcount = (0, state)
val part1 = Range(0, 100)
  .foldLeft(initial) { case (state, iteration) => next(state) }
  ._1

