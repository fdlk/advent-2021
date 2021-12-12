import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day12.txt"))
val connections: List[Set[String]] = input.map(_.split("-").toSet)

type Path = List[String]

def appendToPath(path: Path, connection: Set[String]): Option[Path] =
  Some((connection - path.head).head)
    .filter(nextCave => nextCave.charAt(0).isUpper || !path.contains(nextCave))
    .map(_ :: path)

@tailrec
def part1(paths: Set[Path]): Set[Path] = {
  val next: Set[Path] = paths ++ paths.filter(_.head != "end")
    .flatMap(path => {
      connections.filter(_.contains(path.head))
        .flatMap(appendToPath(path, _))
    })
  if (next == paths) paths.filter(_.head == "end") else part1(next)
}

part1(Set(List("start"))).size