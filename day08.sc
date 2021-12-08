import common.loadPackets

val input = loadPackets(List("day08.txt")).map(_.split(" ").toList)
val outputs = input.map(_.dropWhile(_ != "|").drop(1))

val part1 = outputs.flatten.map(_.length).count(Set(2, 3, 4, 7))

val digits = Array("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
val segments = "abcdefg"

def permute(permutation: String): String => Option[Int] = {
  val replacements = permutation.zip(segments).toMap
  (signal: String) => {
    val permuted = signal.toCharArray.map(replacements).sorted.mkString
    Some(digits.indexOf(permuted)).filter(_ != -1)
  }
}
val permutations = segments.toCharArray.permutations.map(_.mkString).map(permute).toList

def isValidPermutation(permutation: String => Option[Int], digits: List[String]) =
  digits.map(permutation).forall(_.isDefined)

def solve(line: List[String]): Int = {
  val signals = line.filter(_ != "|")
  val output = line.dropWhile(_ != "|").drop(1)
  val permutation = permutations.find(isValidPermutation(_, signals)).get
  output.map(permutation).map(_.get).mkString.toInt
}

val part2 = input.map(solve).sum