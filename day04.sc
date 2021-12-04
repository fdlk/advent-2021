import common.loadPackets
import scala.annotation.tailrec

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

@tailrec
def part1(turn: Int = 0): Int =
  boards.find(hasBingo(_, turn)) match {
    case Some(board) => score(board, turn)
    case None => part1(turn + 1)
  }
part1()

@tailrec
def part2(turn: Int = 0, losers: List[Board] = boards): Int =
  losers.filter(!hasBingo(_, turn)) match {
    case Nil => score(losers.head, turn)
    case stillNoBingo => part2(turn + 1, stillNoBingo)
  }
part2()