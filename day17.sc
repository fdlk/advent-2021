import common.loadPackets

val input = loadPackets(List("day17.txt")).head
val regex = """target area: x=(\d+)..(\d+), y=(-?\d+)\.\.(-?\d+)""".r

val (xmin, xmax, ymin, ymax) = input match {
  case regex(xmin, xmax, ymin, ymax) => (xmin.toInt, xmax.toInt, ymin.toInt, ymax.toInt)
}

case class State(dx: Int, dy: Int, x: Int = 0, y: Int = 0) {
  def next: State = copy(
    dx = math.max(0, dx - 1),
    dy = dy - 1,
    x = x + dx,
    y = y + dy
  )

  val entrenched: Boolean = xmin <= x && xmax >= x && ymin <= y && ymax >= y
  val overshot: Boolean = x > xmax || y < ymin
}

def trajectory(initial: State): Seq[State] = LazyList.iterate(initial)(_.next).takeWhile(!_.overshot)

val validTrajectories = (for (dx <- 1 to xmax;
                             dy <- ymin to 1000)
yield Some(State(dx, dy)).map(trajectory).filter(_.exists(_.entrenched))).flatten

val part1 = validTrajectories.map(_.map(_.y).max).max
val part2 = validTrajectories.size
