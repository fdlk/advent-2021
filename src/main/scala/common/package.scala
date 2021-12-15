import java.io.File
import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.mutable

package object common {

  /** An alias for the `Nothing` type.
   * Denotes that the type should be filled in.
   */
  type ??? = Nothing

  /** An alias for the `Any` type.
   * Denotes that the type should be filled in.
   */
  type *** = Any


  /**
   * Get a child of a file. For example,
   *
   * subFile(homeDir, "b", "c")
   *
   * corresponds to ~/b/c
   */
  def subFile(file: File, children: String*): File = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }

  /**
   * Get a resource from the `src/main/resources` directory. Eclipse does not copy
   * resources to the output directory, then the class loader cannot find them.
   */
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, "src" :: "main" :: "resources" :: resourcePath: _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) / 1000.0 + "s")
    result
  }

  def loadPackets(dictionaryPath: List[String]): List[String] = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }


  def rotateLeft(list: List[Int], by: Int) = list.drop(by) ::: list.take(by)

  case class State(list: List[Int] = (0 until 256).toList, position: Int = 0, skipSize: Int = 0) {
    def tieKnot(length: Int): State = {
      val rotated = rotateLeft(list, position)
      val reversed = rotated.take(length).reverse ::: rotated.drop(length)
      val knotted = rotateLeft(reversed, list.length - position)
      State(knotted, (position + length + skipSize) % list.length, (skipSize + 1) % list.length)
    }

    def denseHash = list.grouped(16)
      .map(_.reduce(_ ^ _))
      .map(x => f"$x%02x")
      .mkString
  }

  def tieKnots(lengths: Seq[Int]): State = lengths.foldLeft(State())(_ tieKnot _)

  def knotHash(input: String): String = {
    val lengths = input.toList.map(_.toInt) ::: List(17, 31, 73, 47, 23)
    tieKnots(List.fill(64)(lengths).flatten).denseHash
  }

  trait Grid[T] {
    def heuristicDistance(from: T, to: T): Int

    def getNeighbours(state: T): List[T]

    def moveCost(from: T, to: T): Int
  }

  def aStarSearch[T](start: T, finish: T, grid: Grid[T]): Option[Int] = {
    val closedVertices = mutable.Set[T]()
    val costFromStart = mutable.Map(start -> 0)
    val estimatedTotalCost = mutable.Map(start -> grid.heuristicDistance(start, finish))
    val openVertices: mutable.Set[T] = mutable.Set(start)

    while (openVertices.nonEmpty) {
      val currentPos = openVertices.minBy(estimatedTotalCost)
      openVertices.remove(currentPos)
      if (currentPos == finish) return Some(estimatedTotalCost(currentPos))
      closedVertices.add(currentPos)
      grid.getNeighbours(currentPos)
        .filterNot(closedVertices.contains)
        .foreach { neighbour =>
          val score = costFromStart(currentPos) + grid.moveCost(currentPos, neighbour)
          if (score < costFromStart.getOrElse(neighbour, Integer.MAX_VALUE)) {
            costFromStart.put(neighbour, score)
            estimatedTotalCost.put(neighbour, score + grid.heuristicDistance(neighbour, finish))
            openVertices.add(neighbour)
          }
        }
    }
    None
  }
}
