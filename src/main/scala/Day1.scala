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


def matchDigitAtStart(s:String) =
  s match
    case s"1${any}"     => Some(1)
    case s"one${any}"   => Some(1)
    case s"2${any}"     => Some(2)
    case s"two${any}"   => Some(2)
    case s"3${any}"     => Some(3)
    case s"three${any}" => Some(3)
    case s"4${any}"     => Some(4)
    case s"four${any}"  => Some(4)
    case s"5${any}"     => Some(5)
    case s"five${any}"  => Some(5)
    case s"6${any}"     => Some(6)
    case s"six${any}"   => Some(6)
    case s"7${any}"     => Some(7)
    case s"seven${any}" => Some(7)
    case s"8${any}"     => Some(8)
    case s"eight${any}" => Some(8)
    case s"9${any}"     => Some(9)
    case s"nine${any}"  => Some(9)
    case _              => None


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


@main
def test(): Unit =
  val part1testinput = """
      1abc2
      pqr3stu8vwx
      a1b2c3d4e5f
      treb7uchet
    """

  println(s"Part 1 Test: ${part1(part1testinput.linesIterator)}")

  val part2testinput = """\
      two1nine
      eightwothree
      abcone2threexyz
      xtwone3four
      4nineeightseven2
      zoneight234
      7pqrstsixteen
    """

  println(s"Part 2 Test: ${part2(part2testinput.linesIterator)}")



@main
def main(): Unit =
  println(s"Part 1: ${part1(Source.fromResource("day1input.txt").getLines())}")
  println(s"Part 2: ${part2(Source.fromResource("day1input.txt").getLines())}")