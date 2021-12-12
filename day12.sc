import common.loadPackets

import scala.annotation.tailrec
import scala.language.implicitConversions

val input = loadPackets(List("day12.txt"))

case class Cave(name: String) {
  def isLarge = name.charAt(0).isUpper
  def isSmall = !isLarge
  def isStart = name == "start"
  def isEnd = name == "end"
}

val connections: Set[Set[Cave]] = input.map(_.split("-").map(Cave).toSet).toSet
type Path = List[Cave]
type PathFilter = Path => Boolean

def neighbors(cave: Cave): Set[Cave] = connections.filter(_.contains(cave)).flatMap(_ - cave)

@tailrec
def findPaths(paths: Set[Path], pathFilter: PathFilter): Set[Path] = {
  val next: Set[Path] = paths ++ paths.filter(!_.head.isEnd)
    .flatMap(path => neighbors(path.head).map(_ :: path))
    .filter(pathFilter)
  if (next == paths) paths.filter(_.head.isEnd) else findPaths(next, pathFilter)
}

val initialPaths = Set(List(Cave("start")))
def filterPart1: PathFilter = {
  case nextCave :: path => nextCave.isLarge || !path.contains(nextCave)
}
val part1 = findPaths(initialPaths, filterPart1).size

def filterPart2: PathFilter = {
  case nextCave :: path =>
    val smallCaves = path.filter(_.isSmall)
    val smallCavesUnique = smallCaves.toSet.size == smallCaves.size
    !nextCave.isStart && (nextCave.isLarge || !path.contains(nextCave) || smallCavesUnique)
}
val part2 = findPaths(initialPaths, filterPart2).size