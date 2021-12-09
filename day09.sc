import common.loadPackets

val input = loadPackets(List("day09.txt")).map(_.split("").toList)

case class Point(x: Int, y: Int) {
  def neighbors() = List(
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

def risk(coord: Point) = depths(coord) + 1

val part1 = depths.keys.toList.filter(
  coord => coord.neighbors()
    .map(neighbor => depths.get(neighbor))
    .filter(_.isDefined)
    .forall(_.get > depths(coord)))
  .map(risk)
  .sum
