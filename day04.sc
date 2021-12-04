import common.loadPackets

val input = loadPackets(List("day04.txt"))

val numbers = input.head.split(",").toList.map(_.toInt)

val boards: List[List[List[Int]]] = input.drop(2).grouped(6).toList
  .map(_.take(5).mkString(" ").toSeq
    .sliding(2, 3).toList.map(_.toString.trim.toInt)
    .grouped(5).toList)

