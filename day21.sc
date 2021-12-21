import common.loadPackets

val input = loadPackets(List("day21.txt")).map(_.split(": ")(1).toInt)

case class State(player1Position: Int = input.head,
                 player2Position: Int = input(1),
                 player1Score: Int = 0,
                 player2Score: Int = 0,
                 dieRolls: Int = 0) {
  def wrap(v: Int): Int = (v - 1) % 10 + 1

  def won: Boolean = player1Score >= 1000 | player2Score >= 1000

  def next(totalDieRoll: Int): State = {
    if (won) this else if (dieRolls / 3 % 2 == 0) {
      val newPos = wrap(player1Position + totalDieRoll)
      copy(player1Position = newPos, player1Score = player1Score + newPos, dieRolls = dieRolls + 3)
    } else {
      val newPos = wrap(player2Position + totalDieRoll)
      copy(player2Position = newPos, player2Score = player2Score + newPos, dieRolls = dieRolls + 3)
    }
  }

  def result: Int = Math.min(player1Score, player2Score) * dieRolls
}

val initial = State(input.head, input(1))

val rolls = Range.inclusive(1, 1000).map(_ % 100).grouped(3).map(_.sum).toList

val game = rolls.foldLeft(initial) { case (state, roll) => state.next(roll) }
val part1 = game.result