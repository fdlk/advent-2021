import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day04.txt"))

type Board = List[List[Int]]
val numbers = input.head.split(",").toVector.map(_.toInt)

val boards: Vector[Board] = input.drop(2).grouped(6).toVector
  .map(_.take(5).mkString(" ").toSeq
    .sliding(2, 3).toList.map(_.toString.trim.toInt)
    .grouped(5).toList)

def hasBingoOnRow(board: Board, drawn: Set[Int]): Boolean =
  board.exists(row => row.forall(drawn.contains))

def hasBingo(board: Board, drawn: Set[Int]): Boolean =
  hasBingoOnRow(board, drawn) ||
    hasBingoOnRow(board.transpose, drawn)

def score(board: Board, drawn: Set[Int]): Int =
  board.flatten.filter(!drawn.contains(_)).sum

@tailrec
def part1(turn: Int = 0): Int = {
  val drawn = numbers.take(turn).toSet
  boards.find(hasBingo(_, drawn)) match {
    case Some(board) => score(board, drawn) * numbers(turn - 1)
    case None => part1(turn + 1)
  }
}

part1()

@tailrec
def part2(turn: Int = 0, leftover: Vector[Board] = boards): Int = {
  val drawn = numbers.take(turn).toSet
  leftover.filter(!hasBingo(_, drawn)) match {
    case Vector() => score(leftover.head, drawn) * numbers(turn - 1)
    case stillNotWon => part2(turn + 1, stillNotWon)
  }
}

part2()