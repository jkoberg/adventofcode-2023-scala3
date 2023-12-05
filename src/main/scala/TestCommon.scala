package us.jkk.aoc2023

trait TestCommon[+A]:
  def input: String
  def fn: Iterator[String] => A
  def expected: A

  def run(): Unit =
    val actual = fn(input.linesIterator)
    if (actual == expected)
      println(s"passed. value $actual")
    else
      println(s"FAILED! expected $expected, got $actual")


case class TestCase(fn: Iterator[String] => Int, expected: Int, input: String) extends TestCommon[Int]
case class LongCase(fn: Iterator[String] => Long, expected: Long, input: String) extends TestCommon[Long]