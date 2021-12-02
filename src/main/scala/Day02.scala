import common.loadPackets

object Day02 extends App {

  case class Position(horizontal: Int = 0, depth: Int = 0, aim: Int = 0) {
    def move: Instruction => Position = {
      case Instruction(direction, amount) =>
        direction match {
          case "up" => copy(depth = depth - amount)
          case "down" => copy(depth = depth + amount)
          case "forward" => copy(horizontal = horizontal + amount)
        }
    }

    def moveWithAim: Instruction => Position = {
      case Instruction(direction, amount) =>
        direction match {
          case "up" => copy(aim = aim - amount)
          case "down" => copy(aim = aim + amount)
          case "forward" => copy(horizontal = horizontal + amount, depth = depth + aim * amount)
        }
    }
  }

  case class Instruction(direction: String, amount: Int)

  val input = loadPackets(List("day02.txt"))
    .map(_.split(" ").toList)
    .map({ case List(a, b) => Instruction(a, b.toInt) })

  val part1 = input.foldLeft(Position())((position, instruction) => position.move(instruction))
  println(part1.horizontal * part1.depth)

  val part2 = input.foldLeft(Position())((position, instruction) => position.moveWithAim(instruction))
  println(part2)
  println(part2.horizontal * part2.depth)
}
