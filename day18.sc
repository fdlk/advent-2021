import common.loadPackets
import fastparse.SingleLineWhitespace._
import fastparse._

import scala.annotation.tailrec
import scala.util.matching.Regex

val input = loadPackets(List("day18.txt"))

@tailrec
def findExplodeIndex(string: String, depth: Int = 0, index: Int = 0): Option[Int] =
  string.headOption match {
    case None => None
    case Some('[') if depth == 4 => Some(index)
    case Some('[') => findExplodeIndex(string.tail, depth + 1, index + 1)
    case Some(']') => findExplodeIndex(string.tail, depth - 1, index + 1)
    case _ => findExplodeIndex(string.tail, depth, index + 1)
  }

val pairRegex = """\[(\d+),(\d+)]""".r
def parsePair(pair: String): (Int, Int) = pair match {
  case pairRegex(left, right) => (left.toInt, right.toInt)
}

val digitsRegex = """\d+""".r

def addToMatchIfPresent(string: String, toAdd: Int, m: Option[Regex.Match]): String =
  m.map(m =>
    List(string.substring(0, m.start),
      m.toString.toInt + toAdd,
      string.substring(m.end)).mkString)
    .getOrElse(string)

def addLeftIfPresent(string: String, toAdd: Int): String =
  addToMatchIfPresent(string, toAdd, digitsRegex.findAllMatchIn(string).toList.lastOption)

def addRightIfPresent(string: String, toAdd: Int): String =
  addToMatchIfPresent(string, toAdd, digitsRegex.findFirstMatchIn(string))

def explode(string: String): Option[String] =
  findExplodeIndex(string).map(from => {
    val to = string.indexOf(']', from) + 1
    val (left, right) = parsePair(string.substring(from, to))
    val before = string.substring(0, from)
    val after = string.substring(to)
    List(addLeftIfPresent(before, left), "0", addRightIfPresent(after, right)).mkString
  })

val splitRegex = """\d{2,}""".r
def split(string: String): Option[String] =
  splitRegex.findFirstMatchIn(string).map(m => {
    val value = m.toString.toInt
    val left = value / 2
    val right = value - left
    List(string.substring(0, m.start), s"[${left},${right}]", string.substring(m.end)).mkString
  })

@tailrec
def reduce(string: String): String = {
  explode(string).orElse(split(string)) match {
    case Some(reduction) => reduce(reduction)
    case None => string
  }
}

def add(left: String, right: String): String =
  reduce(s"[${left},${right}]")

val reduced = input.reduce(add)

object Parser {
  def number[_: P]: P[Int] = CharIn("0-9").rep(1).!.map(_.toInt)
  def expression[_: P]: P[Int] = P("[" ~ (number | expression) ~ "," ~ (number|expression) ~ "]").map{
    case(left, right) => 3 * left + 2 * right
  }
  def magnitude(string: String): Parsed[Int] = parse(string, expression(_))
}

val part1 = Parser.magnitude(reduced)

