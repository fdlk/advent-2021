import common.loadPackets

val input = loadPackets(List("day04.txt"))
type Board = List[List[Int]]

val numbers = input.head.split(",").toVector.map(_.toInt)

val boards: List[Board] = input.drop(2).grouped(6).toList
  .map(_.take(5).mkString(" ").toSeq
    .sliding(2, 3).toList.map(_.toString.trim.toInt)
    .grouped(5).toList)

def drawn(turn: Int): Set[Int] = numbers.take(turn).toSet

def hasBingoOnRow(board: Board, turn: Int): Boolean =
  board.exists(row => row.forall(drawn(turn).contains))

def hasBingo(board: Board, turn: Int): Boolean =
  hasBingoOnRow(board, turn) || hasBingoOnRow(board.transpose, turn)

def score(board: Board, turn: Int): Int =
  board.flatten.filter(!drawn(turn).contains(_)).sum * numbers(turn - 1)

val part1 = numbers.indices
  .flatMap(turn => boards.find(hasBingo(_, turn)).map(score(_, turn)))
  .head

def losers(turn: Int): List[Board] = boards.filter(!hasBingo(_, turn))

val part2 = numbers.indices
  .find(turn => losers(turn) == Nil)
  .map(turn => score(losers(turn - 1).head, turn))
  .get