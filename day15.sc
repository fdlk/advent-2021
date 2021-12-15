import common.{Grid, aStarSearch, loadPackets}

val input = loadPackets(List("day15.txt"))

case class Point(x: Int, y: Int) {
  def neighbors = List(
    copy(x = x - 1),
    copy(x = x + 1),
    copy(y = y - 1),
    copy(y = y + 1)
  )
  def manhattanDistance(to: Point): Int = (x - to.x).abs + (y - to.y).abs
}

val risk: Map[Point, Int] = input.zipWithIndex.flatMap { case (row, y) =>
  row.zipWithIndex.map { case (risk, x) => Point(x, y) -> risk.toString.toInt }
}.toMap

  val start = Point(0, 0)
  val end = Point(input.head.length - 1, input.length - 1)

class ChitonGrid() extends Grid[Point] {
  override def heuristicDistance(from: Point, to: Point) = from.manhattanDistance(to)
  override def getNeighbours(state: Point) = state.neighbors.filter(risk.keySet.contains)
  override def moveCost(from: Point, to: Point) = risk(to)
}

aStarSearch(start, end, new ChitonGrid())

