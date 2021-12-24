case class Connection(room: Char, hallway: Char, distance: Int, blockedBy: String)

val connectionList = List(
  Connection('A', '0', 3, "1"),
  Connection('A', '1', 2, ""),
  Connection('A', '2', 2, ""),
  Connection('A', '3', 4, "2"),
  Connection('A', '4', 6, "23"),
  Connection('A', '5', 8, "234"),
  Connection('A', '6', 9, "2345"),
  Connection('B', '0', 5, "21"),
  Connection('B', '1', 4, "2"),
  Connection('B', '2', 2, ""),
  Connection('B', '3', 2, ""),
  Connection('B', '4', 4, "3"),
  Connection('B', '5', 6, "34"),
  Connection('B', '6', 7, "345"),
  Connection('C', '0', 7, "321"),
  Connection('C', '1', 6, "32"),
  Connection('C', '2', 4, "3"),
  Connection('C', '3', 2, ""),
  Connection('C', '4', 2, ""),
  Connection('C', '5', 4, "4"),
  Connection('C', '6', 5, "45"),
  Connection('D', '0', 9, "4321"),
  Connection('D', '1', 8, "432"),
  Connection('D', '2', 6, "43"),
  Connection('D', '3', 4, "4"),
  Connection('D', '4', 2, ""),
  Connection('D', '5', 2, ""),
  Connection('D', '6', 3, "5"),
)
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

  def moveFromRoomToHallway(room: Char, hallway: Char): State = rooms(room).toList match {
    case aphipod :: leftover =>
      State(rooms.updated(room, leftover.mkString), hallways.updated(hallway, Some(aphipod)))
  }

  def moveFromHallwayToRoom(hallway: Char, room: Char): State = {
    assert(hallways(hallway).isDefined)
    State(rooms.updated(room, room + rooms(room)), hallways.updated(hallway, None))
  }
}

class Burrow extends common.Grid[State] {
  override def heuristicDistance(from: State, to: State): Int =
    from.rooms.flatMap {
      case (room, contents) => contents.filter(_ != room).map(mobility)
    }.sum * 4 +
      from.hallways.map {
        case (hallway, Some(amphipod)) =>
          connections((amphipod, hallway)).distance * mobility(amphipod)
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
    val hallwayOption = halls.find(hallway => from.hallways(hallway) != to.hallways(hallway))
    assert(hallwayOption.isDefined)
    val hallway = hallwayOption.get
    if (from.hallways(hallway).isEmpty) {
      // moved from room to hallway
      val room = rooms.find(room => from.rooms(room) != to.rooms(room)).get
      val extraStep = if (to.rooms(room).isEmpty) 1 else 0
      val aphipod = to.hallways(hallway).get
      (connections((room, hallway)).distance + extraStep) * mobility(aphipod)
    } else {
      val source = from.hallways(hallway)
      assert(source.isDefined)
      // moved from hallway to room
      val aphipod = source.get
      val extraStep = if (from.rooms(aphipod).isEmpty) 1 else 0
      (connections((aphipod, hallway)).distance + extraStep) * mobility(aphipod)
    }
  }
}

val burrow = new Burrow()

val example = State(Map('A' -> "BA", 'B' -> "CD", 'C' -> "BC", 'D' -> "DA"))
val input = State(Map('A' -> "CB", 'B' -> "BC", 'C' -> "DA", 'D' -> "DA"))
/*
#############
#...........#
###C#B#D#D###
  #B#C#A#A#
  #########
 */
val goal = State(rooms.map(room => room -> room.toString * 2).toMap)
val part1 = common.aStarSearch(input, goal, burrow)



