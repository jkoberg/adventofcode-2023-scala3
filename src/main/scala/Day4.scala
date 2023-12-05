package us.jkk.aoc2023

import scala.collection.mutable
import scala.io.Source

object Day4:

  case class Card(index: Int, winning: Set[Int], haves: Set[Int]):
    val matches: Int = haves.intersect(winning).size
    val score: Int = if (matches == 0) 0 else Math.pow(2, matches-1).toInt

  def parseNums(s:String) =
    s.split(" ")
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toSet

  def parseLine(line:String): Option[Card] =
    line match
      case s"Card ${n}: ${wins} | ${haves}" =>
        Some(Card(n.trim.toInt, parseNums(wins), parseNums(haves)))
      case _ =>
        None


  def part1(input: Iterator[String]): Int =
    input.flatMap(parseLine).map(_.score).sum

  def part2(input: Iterator[String]): Int =
    val copies: mutable.Map[Int, Int] = mutable.Map.empty
    var maxCardIndex: Int = 0
    for
      line <- input
      card <- parseLine(line)
    do
      maxCardIndex = maxCardIndex max card.index
      copies.updateWith(card.index) {
        case None => Some(1)
        case Some(numCopies) => Some(numCopies + 1)
      }
      if (card.matches > 0) then {
        for
          copyOf <- 1 to copies(card.index)
          wonCopies <- (card.index + 1) to (card.index + card.matches)
        do
          println(s"Card ${card.index} copy ${copyOf} has score ${card.score}, adding copy to card ${wonCopies}")
          copies.updateWith(wonCopies) {
            case None => Some(1)
            case Some(numCopies) => Some(numCopies + 1)
          }
      }
    println(s"$copies")
    copies.filter((k,v) => k <= maxCardIndex).values.sum



  val testCaseInput =
    """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

  val testCases: Seq[TestCase] = Seq(
    TestCase(part1, 13, testCaseInput),
    TestCase(part2, 30, testCaseInput)
  )

  @main
  def runDay4Tests(): Unit = testCases.foreach(_.run())

  @main
  def runDay4Input(): Unit =
    println(s"Day 4 Part 1: ${part1(Source.fromResource("day4input.txt").getLines())}")
    println(s"Day 4 Part 2: ${part2(Source.fromResource("day4input.txt").getLines())}")