import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day21.txt")).map(_.split(": ")(1).toInt)

val threeThrows =
  (for(x <- Range.inclusive(1, 3);
       y <- Range.inclusive(1, 3);
       z <- Range.inclusive(1, 3)) yield (x + y + z, 1)).groupMapReduce(_._1)(_._2)(_ + _)

case class State(player1Position: Int = input.head,
                 player2Position: Int = input(1),
                 player1Score: Int = 0,
                 player2Score: Int = 0,
                 player1Turn: Boolean = true) {
  def wrap(v: Int): Int = (v - 1) % 10 + 1

  def won: Boolean = player1Score >= 21 | player2Score >= 21

  def winner: Option[Int] =
    if (player1Score >= 21) Some(1)
    else if (player2Score >= 21) Some(2)
    else None

  def next(): Map[State, Int] = {
    if (won) Map(this -> 1)
    else threeThrows.map { case (roll, count) =>
      if (player1Turn) {
        val newPos = wrap(player1Position + roll)
        copy(player1Position = newPos,
          player1Score = player1Score + newPos,
          player1Turn = !player1Turn) -> count
      } else {
        val newPos = wrap(player2Position + roll)
        copy(player2Position = newPos,
          player2Score = player2Score + newPos,
          player1Turn = !player1Turn) -> count
      }
    }
  }
}

val initial: Map[State, Long] = Map(State(input.head, input(1)) -> 1L)
def nextState(universes: Map[State, Long]): Map[State, Long] =
  universes.toList.flatMap {
    case (universe: State, count: Long) => universe.next().view.mapValues(_ * count)
  }.groupMapReduce(_._1)(_._2)(_ + _)

@tailrec
def part2(universes: Map[State, Long]): Long = {
  if (universes.keySet.forall(_.won)) {
    val counts: Map[Int, Long] = universes.groupMapReduce(_._1.winner.get)(_._2)(_ + _)
    counts.values.max
  } else part2(nextState(universes))
}

part2(initial)