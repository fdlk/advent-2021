import common.loadPackets

object Day01 extends App {
  val input = loadPackets(List("day1.txt")).map(_.toInt)

  def countIncreasing: Seq[Int] => Int =
    seq => seq.zip(seq.drop(1)).count { case (a, b) => a < b }

  val part1 = countIncreasing(input)
  val part2 = countIncreasing(input.sliding(3).map(_.sum).toList)

  println(part1, part2)
}