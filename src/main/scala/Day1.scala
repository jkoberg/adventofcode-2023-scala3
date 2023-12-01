package us.jkk.aoc2023

import scala.io.Source

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

def part1(input: Iterator[String]) =
  input
    .filter(_.trim.nonEmpty)
    .map(l => l.filter(c => c.isDigit))
    .map(cs => cs.head.asDigit * 10 + cs.last.asDigit)
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

val digits = Map(
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

def matchDigitAtStart(s:String) =
  for
    (text, value) <- digits
    if s.startsWith(text)
  yield
    value


def scanForDigits(s:String) =
  for
    index <- 0 until s.length
    tail = s.slice(index, s.length)
    digit <- matchDigitAtStart(tail)
  yield
    digit


def part2(input: Iterator[String]) =
  val calibrationFactors =
    for
      line <- input
      digitsForLine = scanForDigits(line)
      if digitsForLine.nonEmpty
    yield
      digitsForLine.head * 10 + digitsForLine.last
  calibrationFactors.sum


case class TestCase(input:String, expected:Int)


def test(testcase:TestCase, f: Iterator[String] => Int): Unit =
  val actual = f(testcase.input.linesIterator)
  if(actual == testcase.expected)
    println(s"passed. value $actual")
  else
    println(s"FAILED! expected ${testcase.expected}, got $actual")



@main
def runTests(): Unit =
  val part1case = TestCase("""
      1abc2
      pqr3stu8vwx
      a1b2c3d4e5f
      treb7uchet
    """, 142)

  test(part1case, part1)

  val part2case = TestCase("""
      two1nine
      eightwothree
      abcone2threexyz
      xtwone3four
      4nineeightseven2
      zoneight234
      7pqrstsixteen
    """, 281)

  test(part2case, part2)



@main
def main(): Unit =
  println(s"Part 1: ${part1(Source.fromResource("day1input.txt").getLines())}")
  println(s"Part 2: ${part2(Source.fromResource("day1input.txt").getLines())}")