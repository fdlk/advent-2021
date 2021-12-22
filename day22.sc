import common.loadPackets

val smallValues = -50 to 50

case class Cuboid(xs: Range, ys: Range, zs: Range) {
  val isSmall: Boolean = List(xs, ys, zs)
    .forall(range => smallValues.contains(range.start) && smallValues.contains(range.end))
  val size: Long = xs.size.toLong * ys.size.toLong * zs.size.toLong
}

case class Reactor(on: Boolean, cuboid: Cuboid) {
  val size: Long = if (on) cuboid.size else -cuboid.size
}

val regex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r
val input = loadPackets(List("day22.txt")).map {
  case regex(boot, xMin, xMax, yMin, yMax, zMin, zMax) =>
    Reactor(boot == "on", Cuboid(xMin.toInt to xMax.toInt,
      yMin.toInt to yMax.toInt,
      zMin.toInt to zMax.toInt))
}

val part1Input: List[Reactor] = input.filter(_.cuboid.isSmall)

def intersectRange(a: Range, b: Range): Range =
  math.max(a.start, b.start) to math.min(a.end, b.end)

def intersectCuboids(a: Cuboid, b: Cuboid): Option[Cuboid] =
  Some(Cuboid(intersectRange(a.xs, b.xs),
    intersectRange(a.ys, b.ys),
    intersectRange(a.zs, b.zs))).filter(_.size > 0)

def combineOnOff(existing: Boolean, action: Boolean): Boolean = (existing, action) match {
  case (true, true) => false // dubbel geteld, er weer van aftrekken
  case (false, false) => true // dubbel afgetrokken, er weer bij optellen
  case (true, false) => false // hij gaat uit, er van aftrekken
  case (false, true) => true // hij gaat aan, er bij optellen
}

def count(reactors: List[Reactor]): Long =
  reactors.foldLeft[List[Reactor]](List.empty) {
    case (acc, action) =>
      val intersections = acc.flatMap(existing =>
        intersectCuboids(action.cuboid, existing.cuboid)
          .map(intersection => Reactor(combineOnOff(existing.on, action.on), intersection)))
      acc ++ intersections ++ List(action).filter(_.on)
  }.map(_.size).sum

val part1 = count(part1Input)
val part2 = count(input)