import common.loadPackets

val input = loadPackets(List("day03.txt"))
  .map(_.split("").map(_.toInt))

val frequencies = input.head.indices
  .map(i =>
    input.map(_.apply(i))
      .groupBy(x => x)
      .view
      .mapValues(_.size)
      .toMap)

def toInteger(bits: Seq[Int]): Int = bits
    .reverse
    .zipWithIndex
    .map {
      case (0, _) => 0
      case (_, index) => Math.pow(2, index).toInt
    }.sum

val gammaRate = toInteger(frequencies.map(x => x.maxBy(_._2)._1))
val epsilonRate = toInteger(frequencies.map(x => x.minBy(_._2)._1))

val part1 = gammaRate * epsilonRate