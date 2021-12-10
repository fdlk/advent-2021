import common.loadPackets

import scala.annotation.tailrec

val input: List[String] = loadPackets(List("day10.txt"))
val openingBrackets = "([{<"
val closingBrackets = ")]}>"

@tailrec
def parse(line: String, stack: List[Char] = Nil): Either[String, Char] = //right for syntax error, left for leftover stack
  (line.headOption, stack.headOption) match {
    case (None, _) => Left(stack.mkString)
    case (Some(opening), _)
      if openingBrackets.contains(opening) =>
      parse(line.tail, opening :: stack)
    case (Some(closing), Some(opening))
      if openingBrackets.indexOf(opening) == closingBrackets.indexOf(closing) =>
      parse(line.tail, stack.tail)
    case (Some(closing), _) => Right(closing)
  }

val syntaxErrorScore = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

val part1 = input.map(parse(_)).flatMap(_.toSeq).map(syntaxErrorScore(_)).sum

def autocompleteScore(stack: String): Long =
  stack.foldLeft(0L)((score, char) => score * 5 + openingBrackets.indexOf(char) + 1)

val autoCompleteScores = input.map(parse(_))
  .flatMap(_.left.toSeq)
  .map(autocompleteScore)
  .sorted
val part2 = autoCompleteScores(autoCompleteScores.length / 2)