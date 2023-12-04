package us.jkk.aoc2023

import scala.io.Source

object Day1:
  /*
  The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration
  value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit
  and the last digit (in that order) to form a single two-digit number.

  For example:

  1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet
  In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

  Consider your entire calibration document. What is the sum of all of the calibration values?
  */

  def part1(input: Iterator[String]): Int =
    input
      .filter(_.trim.nonEmpty)
      .map(_.filter(_.isDigit))
      .map(cs => 10 * cs.head.asDigit + cs.last.asDigit)
      .sum


  /*
  Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two,
  three, four, five, six, seven, eight, and nine also count as valid "digits".

  Equipped with this new information, you now need to find the real first and last digit on each line. For example:

  two1nine
  eightwothree
  abcone2threexyz
  xtwone3four
  4nineeightseven2
  zoneight234
  7pqrstsixteen
  In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

  What is the sum of all of the calibration values?
  */

  val digitRepresentations: Map[String, Int] = Map(
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  val maxDigitLen: Int = digitRepresentations.keys.map(_.length).max

  def scanForDigits(s:String): Seq[Int] =
    for
      index <- 0 until s.length
      substring = s.slice(index, index + maxDigitLen)
      digitValue <- digitRepresentations.collectFirst {
        case (text, value) if substring.startsWith(text) => value
      }
    yield
      digitValue


  def part2(input: Iterator[String]): Int =
    val calibrationFactors =
      for
        line <- input
        digitsForLine = scanForDigits(line)
        if digitsForLine.nonEmpty
      yield
        10 * digitsForLine.head + digitsForLine.last
    calibrationFactors.sum


  val testCases: Seq[TestCase] = Seq(
    TestCase(part1, 142,
      """
        1abc2
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet
      """),

    TestCase(part2, 281,
      """
        two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen
      """)
    )


  @main
  def runTests(): Unit =
    testCases.foreach(_.run())


  @main
  def main(): Unit =
    println(s"Part 1: ${part1(Source.fromResource("day1input.txt").getLines())}")
    println(s"Part 2: ${part2(Source.fromResource("day1input.txt").getLines())}")