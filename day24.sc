import common.loadPackets

val input = loadPackets(List("day24.txt")).grouped(18).toList
val es = input.map(_.drop(4).head.split(" ").last.toInt)
val fs = input.map(_.drop(5).head.split(" ").last.toInt)
val gs = input.map(_.drop(15).head.split(" ").last.toInt)

def step(d: Int, e: Int, f: Int, g: Int, z: Long) =
  if ((z % 26) + f == d) z / e else z / e * 26 + d + g

def getD(z: Long, f: Int): Option[Int] = {
//  println("getD", z, z%26, f)
  Some(((z % 26) + f).toInt).filter(_ > 0).filter(_ < 10)
}

def resolve(ds: List[Int], es: List[Int] = es, fs: List[Int] = fs, gs: List[Int] = gs, z: Long = 0, joker: Boolean = true): Option[List[Int]] = {
  if (es.isEmpty) {
    if (z == 0) Some(Nil)
    else {
      None
    }
  } else {
//    println(ds.headOption, es.head, fs.head, gs.head, joker, z)
    if (es.head == 1 || joker) {
      val nextZ = step(ds.head, es.head, fs.head, gs.head, z)
      resolve(ds.tail, es.tail, fs.tail, gs.tail, nextZ, es.head == 1 && joker).map(ds.head :: _)
    } else {
      getD(z, fs.head).flatMap(d => {
//        println('d', d, fs.head)
        val nextZ = step(d, es.head, fs.head, gs.head, z)
        resolve(ds, es.tail, fs.tail, gs.tail, nextZ, joker).map(d :: _)
      })
    }
  }
}

val part1= (1111111 to 9999999).reverseIterator.map(_.toString).filterNot(_.contains('0'))
  .map(_.toList.map(c => Integer.parseInt(c.toString))).map(ds => resolve(ds, joker=false)).find(_.isDefined)

val part2= (1111111 to 9999999).iterator.map(_.toString).filterNot(_.contains('0'))
  .map(_.toList.map(c => Integer.parseInt(c.toString))).map(ds => resolve(ds, joker=false)).find(_.isDefined)