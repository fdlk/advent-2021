case class Connection(room: Char, hallway: Char, distance: Int, blockedBy: String)

val connectionList = common.loadPackets(List("day23.txt")).map(_.split(" ").toList match {
  case room :: hall :: distance :: obstructedBy =>
    Connection(room.charAt(0), hall.charAt(0), distance.toInt, obstructedBy.mkString)
})

// (room, hallway) -> Connection
val connections: Map[(Char, Char), Connection] =
  connectionList.groupBy(c => (c.room, c.hallway)).view.mapValues(_.head).toMap

/*
#############
#01.2.3.4.56#
###A#B#C#D###
  #A#B#C#D#
  #########
 */

val rooms = "ABCD".toList
val halls = "0123456".toList
val mobility = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)

case class State(rooms: Map[Char, String], hallways: Map[Char, Option[Char]] = halls.map(_ -> None).toMap) {
  def hallwaysReachable(room: Char): List[Char] = connectionList
    .filter(_.room == room)
    .filter(connection => !connection.blockedBy.exists(blocker => hallways(blocker).isDefined))
    .map(_.hallway)

  def moveFromRoomToHallway(room: Char, hallway: Char): State = {
    val contents = rooms(room)
    State(rooms.updated(room, contents.tail), hallways.updated(hallway, Some(contents.head)))
  }

  def moveFromHallwayToRoom(hallway: Char, room: Char): State =
    State(rooms.updated(room, room + rooms(room)), hallways.updated(hallway, None))
}

class Burrow(depth: Int) extends common.Grid[State] {
  val goal = State(rooms.map(room => room -> room.toString * 2).toMap)

  override def heuristicDistance(from: State, to: State): Int =
    from.rooms.flatMap {
      case (room, contents) => contents.filter(_ != room).map(mobility)
    }.sum * 4 +
      from.hallways.map {
        case (hallway, Some(amphipod)) => connections((amphipod, hallway)).distance * mobility(amphipod)
        case _ => 0
      }.sum

  override def getNeighbours(state: State): Iterable[State] =
    state.rooms.flatMap {
      case (room, contents) if contents.exists(_ != room) =>
        state.hallwaysReachable(room)
          .filter(hallway => state.hallways(hallway).isEmpty)
          .map(hallway => state.moveFromRoomToHallway(room, hallway))
      case _ => Nil
    } ++
      state.hallways.flatMap {
        case (hallway, Some(amphipod))
          if state.rooms(amphipod).forall(_ == amphipod) && state.hallwaysReachable(amphipod).contains(hallway) =>
          Some(state.moveFromHallwayToRoom(hallway, amphipod))
        case _ => None
      }

  override def moveCost(from: State, to: State): Int = {
    val hallway = halls.find(hallway => from.hallways(hallway) != to.hallways(hallway)).get
    if (from.hallways(hallway).isEmpty) {
      // moved from room to hallway
      val room = rooms.find(room => from.rooms(room) != to.rooms(room)).get
      val extraSteps = depth - to.rooms(room).length
      val aphipod = to.hallways(hallway).get
      (connections((room, hallway)).distance + extraSteps) * mobility(aphipod)
    } else {
      // moved from hallway to room
      val aphipod = from.hallways(hallway).get
      val extraSteps = depth - from.rooms(aphipod).length
      (connections((aphipod, hallway)).distance + extraSteps) * mobility(aphipod)
    }
  }
}

val inputPart1 = State(Map('A' -> "CB", 'B' -> "BC", 'C' -> "DA", 'D' -> "DA"))
/*
#############
#...........#
###C#B#D#D###
  #B#C#A#A#
  #########
 */
val goalPart1 = State(rooms.map(room => room -> room.toString * 2).toMap)
val part1 = common.aStarSearch(inputPart1, goalPart1, new Burrow(2))

val inputPart2 = State(Map('A' -> "CDDB", 'B' -> "BCBC", 'C' -> "DBAA", 'D' -> "DACA"))
/*
#############
#...........#
###C#B#D#D###
  #D#C#B#A#
  #D#B#A#C#
  #B#C#A#A#
  #########
 */
val goalPart2 = State(rooms.map(room => room -> room.toString * 4).toMap)
val part2 = common.aStarSearch(inputPart2, goalPart2, new Burrow(4))