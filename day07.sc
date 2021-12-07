import common.loadPackets
val input = loadPackets(List("day07.txt")).head.split(",").map(_.toInt).toList

val part1 = Range.inclusive(input.min, input.max)
  .map(candidate => input.map(x => (x-candidate).abs).sum).min

def fuelCost(steps: Int): Int = steps * (steps + 1) / 2

val part2 = Range.inclusive(input.min, input.max)
  .map(candidate => input.map(x => fuelCost((x-candidate).abs)).sum).min