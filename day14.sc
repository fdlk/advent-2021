import common.loadPackets

val input = loadPackets(List("day14.txt"))

val initialString = input.head
val insertions: Map[String, Char] = input.drop(2).map(_.split(" -> ")).map {
  case Array(from, to) => from -> to.charAt(0)
}.toMap

def next(state: Map[String, Long]): Map[String, Long] = {
  val mapped: List[(String, Long)] = state.toList.flatMap { case pair -> amount =>
    val insertion = insertions(pair)
    val left = List(pair.charAt(0), insertion).mkString
    val right = List(insertion, pair.charAt(1)).mkString
    List(left -> amount, right -> amount)
  }
  mapped.groupMapReduce(_._1)(_._2)(_ + _)
}

val initialPairs = initialString.toList.sliding(2).toList.map(_.mkString -> 1L)
  .groupMapReduce(_._1)(_._2)(_ + _)
val iterations = LazyList.iterate(initialPairs)(next)

def solve(n: Int) = {
  val nth = iterations.drop(n).head
    .toList
    .flatMap{case (pair, amount) => List(pair.charAt(0) -> amount, pair.charAt(1) -> amount)}
  val frequencies = (initialString.head -> 1L :: initialString.last -> 1L :: nth)
    .groupMapReduce(_._1)(_._2)(_ + _)
    .values.map(_ / 2)
  frequencies.max - frequencies.min
}

val part1 = solve(10)
val part2 = solve(40)