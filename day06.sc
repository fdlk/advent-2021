import common.loadPackets

type Population = Map[Int, Long]
val input = loadPackets(List("day06.txt")).head.split(",").map(_.toInt).toList

val initialState: Population = input.groupBy(identity).view.mapValues(_.size.toLong).toMap

def next(state: Population): Population = state.map {
  case (timer, amount) if timer == 0 => List((6, amount), (8, amount))
  case (timer, amount) => List((timer - 1, amount))
}.toList.flatten
  .foldLeft(Map[Int, Long]()) {
    case (map, (timer, amount)) =>
      map + (timer -> (map.getOrElse(timer, 0L) + amount))
  }

val part1 = Range(0, 80).foldLeft(initialState)((state, _) => next(state)).values.sum
val part2 = Range(0, 256).foldLeft(initialState)((state, _) => next(state)).values.sum

