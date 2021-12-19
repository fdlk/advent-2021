import common.loadPackets

import scala.annotation.tailrec

type Point = (Int, Int, Int)

def outer(a: Point, b: Point): Point = (a, b) match {
  case ((ax, ay, az), (bx, by, bz)) => (ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx)
}

def inner(a: Point, b: Point): Int = (a, b) match {
  case ((ax, ay, az), (bx, by, bz)) => ax * bx + ay * by + az * bz
}

def add(a: Point, b: Point): Point = (a, b) match {
  case ((ax, ay, az), (bx, by, bz)) => (ax + bx, ay + by, az + bz)
}

def minus(a: Point): Point = a match {
  case (x, y, z) => (-x, -y, -z)
}

case class Orientation(x: Point, y: Point) {
  val z = outer(x, y)

  def rotate(p: Point): Point = (inner(x, p), inner(y, p), inner(z, p))
}

val axes = Set((1, 0, 0), (0, 1, 0), (0, 0, 1))
val orientations = axes.flatMap(x =>
  (axes - x).flatMap(y => List(Orientation(x, y), Orientation(x, minus(y)), Orientation(minus(x), y), Orientation(minus(x), minus(y))))
).toList

orientations.size

case class Scanner(orientation: Orientation, position: Point = (0, 0, 0)) {
  def transform(beacon: Point): Point = add(position, orientation.rotate(beacon))
}

def findOverlap(beacons: Set[Point], otherBeacons: Set[Point]): Option[Scanner] = {
  val options: List[Scanner] = orientations.flatMap(orientation => {
    otherBeacons.flatMap(otherBeacon => {
      val offset = orientation.rotate(otherBeacon)
      beacons.map(beacon => Scanner(orientation, add(beacon, minus(offset))))
    })
  })
  options.find(scanner => otherBeacons.map(scanner.transform).intersect(beacons).size >= 12)
}

val pointRegex = """(-?\d+),(-?\d+),(-?\d+)""".r
val input = loadPackets(List("day19.txt")).filter(_ != "")
  .map {
    case pointRegex(x, y, z) => Some((x.toInt, y.toInt, z.toInt))
    case _ => None
  }.foldLeft[List[Set[Point]]](Nil) {
  case (acc, Some(point)) => acc.head + point :: acc.tail
  case (acc, None) => Set[Point]() :: acc
}

@tailrec
def unify(beacons: List[Set[Point]], scanners: List[Scanner] = List(Scanner(Orientation((1, 0, 0), (0, 1, 0)), (0, 0, 0)))): (Set[Point], List[Scanner]) = beacons match {
  case head :: Nil => (head, scanners)
  case head :: rest => val other = rest.find(other => findOverlap(head, other).isDefined).get
    val scanner = findOverlap(head, other).get
    unify(head ++ other.map(scanner.transform) :: rest.filter(_ != other), scanner :: scanners)
}

val (beacons, scanners) = unify(input)
val part1 = beacons.size
val part2 = scanners.map(_.position).combinations(2).map {
  case List((x1, y1, z1), (x2, y2, z2)) => (x1 - x2).abs + (y1 - y2).abs + (z1 - z2).abs
}.max