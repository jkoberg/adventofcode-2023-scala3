package us.jkk.aoc2023

import scala.io.Source


def readInput(day:Int): Iterator[String] =
  Source.fromResource(s"day${day}input.txt").getLines()


case class TestCase[A](
    fn: Iterator[String] => A,
    expected: A,
    input: String
  ):
  def run(): Unit =
    val actual = fn(input.linesIterator)
    if (actual == expected)
      println(s"passed. value $actual")
    else
      println(s"FAILED! expected $expected, got $actual")
