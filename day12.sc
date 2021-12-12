import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day12.txt"))
val connections: List[Set[String]] = input.map(_.split("-").toSet)

type Path = List[String]
type PathFilter = (Path, String) => Boolean

def filterPart1: PathFilter = (path, nextCave) =>
  nextCave.charAt(0).isUpper || !path.contains(nextCave)

def appendToPath(path: Path, connection: Set[String], pathFilter: PathFilter): Option[Path] =
  Some((connection - path.head).head)
    .filter(pathFilter(path, _))
    .map(_ :: path)

@tailrec
def findPaths(paths: Set[Path], pathFilter: PathFilter): Set[Path] = {
  val next: Set[Path] = paths ++ paths.filter(_.head != "end")
    .flatMap(path => connections.filter(_.contains(path.head))
      .flatMap(appendToPath(path, _, pathFilter)))
  if (next == paths) paths.filter(_.head == "end") else findPaths(next, pathFilter)
}

val part1 = findPaths(Set(List("start")), filterPart1).size

def filterPart2: PathFilter = (path, nextCave) => {
  val smallCaves = path.filter(_.charAt(0).isLower)
  val smallCavesUnique = smallCaves.toSet.size == smallCaves.size
  nextCave != "start" && (nextCave.charAt(0).isUpper || !path.contains(nextCave) || smallCavesUnique)
}

val part2 = findPaths(Set(List("start")), filterPart2).size