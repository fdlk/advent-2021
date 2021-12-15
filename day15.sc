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

class ChitonGrid extends Grid[Point] {
  override def heuristicDistance(from: Point, to: Point) = from.manhattanDistance(to)

  override def getNeighbours(state: Point) = state.neighbors.filter(risk.keySet)

  override def moveCost(from: Point, to: Point) = risk(to)
}

val part1 = aStarSearch(Point(0, 0), Point(input.head.length - 1, input.length - 1), new ChitonGrid)

val width = input.head.length
val height = input.length
val riskPart2: Map[Point, Int] = (for (x <- input.head.indices;
                                       y <- input.indices;
                                       gridX <- 0 until 5;
                                       gridY <- 0 until 5) yield
  Point(x + gridX * width, y + gridY * width) -> ((risk(Point(x, y)) + gridX + gridY - 1) % 9 + 1)
  ).toMap

class ExtendedChitonGrid extends ChitonGrid {
  override def getNeighbours(state: Point) = state.neighbors.filter(riskPart2.keySet)

  override def moveCost(from: Point, to: Point) = riskPart2(to)
}

val part2 = aStarSearch(Point(0, 0), Point(width * 5 - 1, height * 5 - 1), new ExtendedChitonGrid)
