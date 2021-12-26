import common.loadPackets

val input: List[String] = loadPackets(List("day25.txt"))
val ymax = input.indices.max
val xmax = input.head.indices.max

case class Coord(x: Int, y: Int) {
  def moveEast: Coord = copy(x = if (x == xmax) 0 else x + 1)
  def moveSouth: Coord = copy(y = if (y == ymax) 0 else y + 1)
}

case class Cucumber(coord: Coord, direction: Char) {
  def move: Cucumber =
    if (direction == 'v') copy(coord = coord.moveSouth)
    else copy(coord = coord.moveEast)
}

val initial = input.zipWithIndex.flatMap {
  case (row, y) =>
    row.zipWithIndex.map {
      case (value, x) if value != '.' => Some(Cucumber(Coord(x, y), value))
      case _ => None
    }
}.flatten.toSet

def move(cucumbers: Set[Cucumber], direction: Char): Set[Cucumber] = {
  val taken = cucumbers.map(_.coord)
  cucumbers.map(cucumber =>
    Some(cucumber).filter(_.direction == direction)
      .map(_.move)
      .filterNot(cucumber => taken(cucumber.coord))
      .getOrElse(cucumber))
}

def printed(cucumbers: Set[Cucumber]): String =
  (for(y <- input.indices;
    x<-input.head.indices)
    yield cucumbers.find(_.coord == Coord(x, y)).map(_.direction).getOrElse('.')).grouped(input.head.length).map(_.mkString).mkString("\n")

def next(cucumbers: Set[Cucumber]) = move(move(cucumbers, '>'), 'v')
def iterations = LazyList.iterate(initial)(next)
val part1 = iterations.zip(iterations.tail).indexWhere(pair => pair._1 == pair._2) + 1