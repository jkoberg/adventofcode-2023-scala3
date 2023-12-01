package us.jkk.aoc2023

import scala.io.Source

  /*
The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

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
    .map(l => l.filter(c => c.isDigit))
    .map(cs => cs.head.asDigit * 10 + cs.last.asDigit)
    .sum

/*
  Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

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


def part2(input: Iterator[String]) =
  input
    .map(l =>
      l.replaceAll("(one|two|three|four|five|six|seven|eight|nine)", " $1 ")
        .replace("one", "1")
        .replace("two", "2")
        .replace("three", "3")
        .replace("four", "4")
        .replace("five", "5")
        .replace("six", "6")
        .replace("seven", "7")
        .replace("eight", "8")
        .replace("nine", "9")
      )
    .map(l => l.filter(c => c.isDigit))
    .map(cs => cs.head.asDigit * 10 + cs.last.asDigit)
    .sum

@main
def test() =
  val part1testinput =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin
  println(s"Part 1 Test: ${part1(part1testinput.linesIterator)}")

  val part2testinput =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin
  println(s"Part 2 Test: ${part2(part2testinput.linesIterator)}")

@main
def main() =
  println(s"Part 1: ${part1(Source.fromResource("day1input.txt").getLines())}")
  println(s"Part 2: ${part2(Source.fromResource("day1input.txt").getLines())}")