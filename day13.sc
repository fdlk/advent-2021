import common.loadPackets

val input = loadPackets(List("day13.txt"))

case class Point(x: Int, y: Int) {
  def foldAlongY(fold: Int) =
    if (y < fold) this else copy(y = fold - (y - fold))

  def foldAlongX(fold: Int) =
    if (x < fold) this else copy(x = fold - (x - fold))
}

val (pointsInput, foldInput) = input.splitAt(input.indexOf(""))
val initialPoints = pointsInput.map(_.split(",").toList)
  .map { case List(x, y) => Point(x.toInt, y.toInt) }.toSet

val regex = """fold along ([xy])=(\d+)""".r
def fold: (Set[Point], String) => Set[Point] = {
  case (points, regex(xy, line)) => xy match {
    case "x" => points.map(_.foldAlongX(line.toInt))
    case "y" => points.map(_.foldAlongY(line.toInt))
  }
}

val part1 = fold(initialPoints, foldInput.tail.head).size

val part2 = foldInput.tail.foldLeft(initialPoints)(fold)

def print(points: Set[Point]) = {
  val xs = points.map(_.x)
  val ys = points.map(_.y)
  (ys.min to ys.max).map(y =>
    (xs.min to xs.max).map(x =>
      if (points.contains(Point(x, y))) "#" else " ").mkString
  ).mkString("\n")
}
print(part2)
