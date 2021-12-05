import common.loadPackets

val input = loadPackets(List("day05.txt"))

case class Point(x: Int, y: Int)

case class Vent(x1: Int, y1: Int, x2: Int, y2: Int) {
  def isHorizontal: Boolean = x1 == x2

  def isVertical: Boolean = y1 == y2

  def isDiagonal: Boolean = !isVertical && !isHorizontal

  def points: Seq[Point] = {
    val dx = Math.signum(x2 - x1).toInt
    val dy = Math.signum(y2 - y1).toInt
    Range.inclusive(0, Math.max(Math.abs(x2 - x1), Math.abs(y2 - y1)))
      .map(step => Point(x1 + dx * step, y1 + dy * step))
  }
}

val regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
val vents = input.map({ case regex(x0, y0, x1, y1) => Vent(x0.toInt, y0.toInt, x1.toInt, y1.toInt) })

def countOverlaps(vents: List[Vent]): Int =
  vents.flatMap(_.points).groupBy(x => x).view.mapValues(_.size).count(_._2 > 1)

val part1 = countOverlaps(vents.filter(!_.isDiagonal))
val part2 = countOverlaps(vents)