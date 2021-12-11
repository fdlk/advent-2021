import common.loadPackets

val input: List[String] = loadPackets(List("day10.txt"))
val openingBrackets = "([{<"
val closingBrackets = ")]}>"

//left for syntax error, right for leftover stack
type ParseResult = Either[Char, List[Char]]
def parse(line: String) = line.foldLeft[ParseResult](Right(Nil)) {
  case (syntaxError, _) if syntaxError.isLeft => syntaxError
  case (Right(stack), char) if openingBrackets.contains(char) => Right(char :: stack)
  case (Right(opening :: stack), closing)
    if openingBrackets.indexOf(opening) == closingBrackets.indexOf(closing) => Right(stack)
  case (_, unmatched) => Left(unmatched)
}
val parsed = input.map(parse)

val syntaxErrorScore = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
val part1 = parsed.flatMap(_.left.toSeq).map(syntaxErrorScore(_)).sum

def autocompleteScore(stack: List[Char]): Long =
  stack.foldLeft(0L)((score, char) => score * 5 + openingBrackets.indexOf(char) + 1)

val autoCompleteScores = parsed.flatMap(_.toSeq).map(autocompleteScore).sorted
val part2 = autoCompleteScores(autoCompleteScores.length / 2)