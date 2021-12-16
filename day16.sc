import common.loadPackets

val input = loadPackets(List("day16.txt")).head

type Bit = Int

val bits: List[Bit] = input.split("")
  .map(Integer.parseInt(_, 16))
  .map(_.toBinaryString)
  .map(_.reverse.padTo(4, '0').reverse)
  .flatMap(_.split(""))
  .map(_.toInt).toList

def getBitValue(bits: List[Bit]) = Integer.parseInt(bits.mkString, 2)

sealed abstract class Packet

case class LiteralPacket(version: Int, typeID: Int, payload: Long) extends Packet

case class OperatorPacket(version: Int, typeID: Int, payload: List[Packet]) extends Packet

def parsePacket(input: List[Bit]): (Int, Packet) = {
  def parsePackets(input: List[Bit]): List[Packet] = {
    val (bitsRead, packet) = parsePacket(input)
    val leftover = input.drop(bitsRead)
    if (leftover.isEmpty) List(packet)
    else packet :: parsePackets(leftover)
  }

  def parseLiteralPacket(version: Int, typeID: Int, body: List[Bit]): (Int, LiteralPacket) = {
    val grouped = body.grouped(5).toList
    val numGroups = grouped.zipWithIndex.find(_._1.head == 0).get._2 + 1
    val payload = grouped.take(numGroups)
      .map(_.tail)
      .map(getBitValue)
      .foldLeft(0L)((sum, digit) => sum * 16 + digit)
    (6 + 5 * numGroups, LiteralPacket(version, typeID, payload))
  }

  def parseOperatorPacket(version: Int, typeID: Int, body: List[Bit]): (Int, OperatorPacket) = {
    val lengthTypeID = body.head
    val lengthAndPackets = body.tail
    if (lengthTypeID == 0) {
      val (lengthBits, rest) = lengthAndPackets.splitAt(15)
      val packetLength = getBitValue(lengthBits)
      val packetBits = rest.take(packetLength)
      (6 + 1 + 15 + packetLength, OperatorPacket(version, typeID, parsePackets(packetBits)))
    } else {
      val (lengthBits, rest) = lengthAndPackets.splitAt(11)
      val numPackets = getBitValue(lengthBits)
      val (packetLength, packetStack) = Range(0, numPackets)
        .foldLeft[(Int, List[Packet])]((0, Nil)) {
          case ((count, packets), _) =>
            val (packetBits, packet) = parsePacket(rest.drop(count))
            (count + packetBits, packet :: packets)
        }
      (6 + 1 + 11 + packetLength, OperatorPacket(version, typeID, packetStack.reverse))
    }
  }

  val header = input.grouped(3).take(2).map(getBitValue).toList
  val version = header.head
  val typeID = header.tail.head
  val body = input.drop(6)
  typeID match {
    case 4 => parseLiteralPacket(version, typeID, body)
    case _ => parseOperatorPacket(version, typeID, body)
  }
}

val parsed = parsePacket(bits)._2

def versionSum(packet: Packet): Int = packet match {
  case LiteralPacket(version, _, _) => version
  case OperatorPacket(version, _, payload) => version + payload.map(versionSum).sum
}

val part1 = versionSum(parsed)

def compute(packet: Packet): Long = packet match {
  case LiteralPacket(_, _, value) => value
  case OperatorPacket(_, 0, values) => values.map(compute).sum
  case OperatorPacket(_, 1, values) => values.map(compute).product
  case OperatorPacket(_, 2, values) => values.map(compute).min
  case OperatorPacket(_, 3, values) => values.map(compute).max
  case OperatorPacket(_, 5, values) => values.map(compute) match {
    case List(a, b) if a > b => 1
    case _ => 0
  }
  case OperatorPacket(_, 6, values) => values.map(compute) match {
    case List(a, b) if a < b => 1
    case _ => 0
  }
  case OperatorPacket(_, 7, values) => values.map(compute) match {
    case List(a, b) if a == b => 1
    case _ => 0
  }
}

val part2 = compute(parsed)