import common.loadPackets

val input = loadPackets(List("day03.txt"))
  .map(_.split("").map(_.toInt))

def frequencies: List[Array[Int]] => IndexedSeq[Map[Int, Int]] =
  rows => rows.head.indices.map(i =>
    rows.map(_.apply(i))
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

val mostCommon = frequencies(input).map(x => x.maxBy(_._2)._1)
val gammaRate = toInteger(mostCommon)
val leastCommon = frequencies(input).map(x => x.minBy(_._2)._1)
val epsilonRate = toInteger(leastCommon)

val part1 = gammaRate * epsilonRate

val filterRows: (Int, List[Array[Int]], List[Array[Int]] => IndexedSeq[Int]) => Array[Int] = {
  case (_, rows, _) if rows.size == 1 => rows.head
  case (index, rows, bits) => filterRows(
    index + 1,
    rows.filter(row => row(index) == bits(rows)(index)),
    bits
  )
}

val oxygenGeneratorRating = toInteger(filterRows(0, input,
  rows => frequencies(rows).map(x => if (x.getOrElse(0, 0) == x.getOrElse(1, 0)) 1 else x.maxBy(_._2)._1)))
val co2Rating = toInteger(filterRows(0, input,
  rows => frequencies(rows).map(x => if (x.getOrElse(0, 0) == x.getOrElse(1, 0)) 0 else x.minBy(_._2)._1)))

val part2 = oxygenGeneratorRating * co2Rating