import common.loadPackets
import scala.annotation.tailrec

val input = loadPackets(List("day09.txt")).map(_.split("").toList)

case class Point(x: Int, y: Int) {
  def neighbors = List(
    copy(x = x-1),
    copy(x = x+1),
    copy(y = y-1),
    copy(y = y+1)
  )
}

val depths = input.zipWithIndex
  .flatMap { case (row, y) => row.zipWithIndex
    .map { case (depth, x) => Point(x, y) -> depth.toInt}
  }.toMap

def risk(coord: Point):Int = depths(coord) + 1

val lowPoints = depths.keys.filter(
  coord => coord.neighbors
    .map(neighbor => depths.get(neighbor))
    .filter(_.isDefined)
    .forall(_.get > depths(coord)))

val part1 = lowPoints.map(risk).sum

@tailrec
def basin(coords: Set[Point]): Set[Point] = {
  val next:Set[Point] = coords ++
    coords.flatMap(_.neighbors)
      .filter(neighbor => depths.get(neighbor).exists(_ != 9))
  if (next == coords) coords else basin(next)
}

val part2 = lowPoints.map(point => basin(Set(point)).size)
  .toList.sorted.reverse.take(3)
  .product
