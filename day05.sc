import common.loadPackets

val input = loadPackets(List("day05.txt"))

case class Vent(x1: Int, y1: Int, x2: Int, y2: Int) {
  val dx = (x2 - x1).sign
  val dy = (y2 - y1).sign
  def isDiagonal: Boolean = dx * dy != 0
  def points: Seq[(Int, Int)] =
    Range.inclusive(0, math.max((x2 - x1).abs, (y2 - y1).abs))
      .map(step => (x1 + dx * step, y1 + dy * step))
}

val regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
val vents = input map {
  case regex(x0, y0, x1, y1) => Vent(x0.toInt, y0.toInt, x1.toInt, y1.toInt)
}

def countOverlaps(vents: List[Vent]): Int =
  vents.flatMap(_.points).groupBy(identity).values.count(_.size > 1)

val part1 = countOverlaps(vents.filter(!_.isDiagonal))
val part2 = countOverlaps(vents)