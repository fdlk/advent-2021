import common.loadPackets

val input = loadPackets(List("day20.txt"))
val imageEnhancementAlgorithm = input.head.map({
  case '#' => 1
  case '.' => 0
})

case class Point(x: Int, y: Int) {
  def neighbors: IndexedSeq[Point] =
    for (dy <- -1 to 1;
         dx <- -1 to 1)
    yield copy(x + dx, y + dy)
}
type Image = Map[Point, Int]

val image: Image = input.drop(2).zipWithIndex.flatMap {
  case (row, y) => row.zipWithIndex.map {
    case ('#', x) => Point(x, y) -> 1
    case ('.', x) => Point(x, y) -> 0
  }
}.toMap

def enhance(point: Point, image: Image, defaultLit: Int): Int = {
  val index = point.neighbors.map(neighbor => image.getOrElse(neighbor, defaultLit)).mkString
  imageEnhancementAlgorithm(Integer.parseInt(index, 2))
}

type State = (Image, Int)

def next(state: State): State = state match {
  case (image, defaultLit) =>
    val xs = image.keySet.map(_.x)
    val ys = image.keySet.map(_.y)
    val nextImage = (for (x <- xs.min - 2 to xs.max + 2;
          y <- ys.min - 2 to ys.max + 2)
    yield Point(x, y))
        .map(p => (p, enhance(p, image, defaultLit))).toMap
    (nextImage, 1-defaultLit)
}

val initial: State = (image, 0)
val (part1,_) = next(next(initial))
part1.values.count(_ == 1)

def printImage(image: Map[Point, Int]): Unit = {
  val xs = image.keySet.map(_.x)
  val ys = image.keySet.map(_.y)

  val points = for (y <- ys.min to ys.max;
        x <- xs.min to xs.max)
  yield if(image(Point(x,y)) == 1) '#' else '.'

  points.grouped(xs.max - xs.min + 1)
    .foreach(x => println(x.mkString))
}

printImage(part1)
val iterations = LazyList.iterate(initial)(next)

val (part2,_) = iterations.drop(50).head
part2.values.count(_ == 1)
printImage(part2)