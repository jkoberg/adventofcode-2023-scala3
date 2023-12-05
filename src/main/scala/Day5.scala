package us.jkk.aoc2023

import scala.io.Source

object Day5:
  
  case class MapEntry(sourceStart: Long, destStart: Long, count: Long)
  
  case class SourceToDestMap(mappings: Seq[MapEntry]):
    def lookup(key: Long): Long =
      mappings
        .collectFirst {
          case e if key >= e.sourceStart && key < e.sourceStart + e.count =>
            (key - e.sourceStart) + e.destStart
          }
        .getOrElse(key)

  case class PuzzleModel(inputs: Seq[(Long, Long)], mapChain: Seq[SourceToDestMap]):
    def processInput(input: Long) =
      mapChain.foldLeft(input) { (i, map) => map.lookup(i) }
      
    def processInputs: Seq[Long] =
      for
        (start, count) <- inputs
        _ = println(s"${count} seeds from ${start}")
        seed <- start until (start + count)
      yield
        processInput(seed)
  
  def parseInput(input: Iterator[String], inputMapping: Seq[Long] => Seq[(Long, Long)]): PuzzleModel =
    val wholeInput = input.mkString("\n")
    val sections = wholeInput.split("\n\n")
    val inputs = sections.head match {
      case s"${name}: ${numbers}" => numbers.split(" ").filter(_.nonEmpty).map(_.toLong)
      case _ => sys error "Parsing error in inputs"
    }
    val maps =
      for
        section <- sections.tail
      yield
        SourceToDestMap(section.split("\n").tail.map {
          case s"${outputStart} ${inputStart} ${count}" => MapEntry(inputStart.toLong, outputStart.toLong, count.toLong)
          case _ => sys error "Parsing error in mapping"
        })
    PuzzleModel(inputMapping(inputs), maps)
    

  def part1(input: Iterator[String]): Long =
    val model = parseInput(input, is => is.map(i => (i, 1L)))
    model.processInputs.min

  def part2(input: Iterator[String]): Long =
    val model = parseInput(input, is => is.grouped(2).map { case Seq(i1, i2) => (i1, i2)}.toSeq)
    model.processInputs.min

  val testCaseInput =
    """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

  val testCases: Seq[TestCommon[Any]] = Seq(
    LongCase(part1, 35, testCaseInput),
    LongCase(part2, 46, testCaseInput)
  )

  @main
  def runDay5Tests(): Unit = testCases.foreach(_.run())

  @main
  def runDay5Input(): Unit =
    println(s"Day 5 Part 1: ${part1(Source.fromResource("day5input.txt").getLines())}")
    println(s"Day 5 Part 2: ${part2(Source.fromResource("day5input.txt").getLines())}")
