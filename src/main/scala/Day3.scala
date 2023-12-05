package us.jkk.aoc2023

import scala.annotation.tailrec
import scala.io.Source

object Day3:

  type Row = Int
  type Col = Int
  type PartNumberT = Int
  type PartSymbolT = String


  extension (c: Char)
    def isPartSym = c != '.' && !c.isDigit

  enum ScanFindings:
    case PartNumber(row: Row, startcol: Col, endcol: Col, value: PartNumberT)
    case PartSymbol(row: Row, col: Col, symbol: PartSymbolT)

  enum LineScanState:
    case Idle
    case InNumber(start: Col, digits: String)


  import LineScanState._
  import ScanFindings._

  // find all part symbols - non-digit and non-. characters, output their row/col coords
  // find all digit strings, output their row and column ranges
  // enumerate the neighborhood of each found "part" symbol
  // for each neighborhood, if there is a digit string on the same row and where the range of
  // digit position columns intersects the neighborhood, output the digit string
  //
  // or
  //
  // For every x,y that is part of a digit sequence, set a map of (x,y) => part number
  // for every part symbol, compute neighborhood, look into map for each neighborhood member

  // as I scan a line, I am either
  //   was not on a digit, am not on a digit -> idle
  //   was not on a digit, am now on a digit -> InNumber(start = current index, accum = current char)
  //   was on a digit, am now on a digit -> InNumber(start = whatever, accum = accum + current char)
  //   was on a digit, am now not on a digit -> yield PartNumber(start = x1, end = current index -1, value = accum.toint)
  //   will need to add a space to the end of every line to avoid special case at edge - OR maintain end of accum in correct state


  @tailrec
  def scanLine_rec(
      line:   String,
      rowidx: Row,
      colidx: Col = 0,
      state:  LineScanState = Idle,
      accum:  Vector[ScanFindings] = Vector.empty
    ): Vector[ScanFindings] = {
    (state, line.headOption) match
      case (Idle, None) => accum
      case (InNumber(s, ds), None) => accum :+ PartNumber(rowidx, s, colidx - 1, ds.toInt)
      case (Idle, Some('.')) => scanLine_rec(line.tail, rowidx, colidx + 1, Idle, accum)
      case (Idle, Some(c)) if c.isPartSym => scanLine_rec(line.tail, rowidx, colidx + 1, Idle, accum :+ PartSymbol(rowidx, colidx, c.toString))
      case (Idle, Some(c)) if c.isDigit => scanLine_rec(line.tail, rowidx, colidx + 1, InNumber(colidx, c.toString), accum)
      case (InNumber(s, ds), Some(c)) if c.isDigit => scanLine_rec(line.tail, rowidx, colidx + 1, InNumber(s, ds :+ c), accum)
      case (InNumber(s, ds), Some(c)) if !c.isDigit => scanLine_rec(line, rowidx, colidx, Idle, accum :+ PartNumber(rowidx, s, colidx - 1, ds.toInt))
  }

  def neighborhood(rowidx:Row, colidx:Col) =
    for
      rowoffset <- Seq(-1, 0, 1)
      coloffset <- Seq(-1, 0, 1)
      if (rowoffset, coloffset) != (0,0)
      row = rowidx + rowoffset
      col = colidx + coloffset
      if row != -1
      if col != -1
    yield
      (row, col)


  type PartMap = Map[(Row, Col, PartSymbolT), Set[(Row, Col, PartNumberT)]]

  def createPartMap(input:Iterator[String]): PartMap =
    var partNumbers: Map[(Int, Int), PartNumber] = Map.empty
    var partSymbols: Vector[PartSymbol] = Vector.empty
    for
      pair <- input.zipWithIndex
      (line, rowidx) = pair
      if line.trim.nonEmpty
      finding <- scanLine_rec(line, rowidx)
    do
      finding match
        case pn: PartNumber =>
          for
            col <- pn.startcol to pn.endcol
          do
            partNumbers = partNumbers + ((pn.row, col) -> pn)
        case ps: PartSymbol =>
          partSymbols = partSymbols :+ ps
    var partMap: Map[(Int, Int, String), Set[(Int, Int, Int)]] = Map.empty
    for
      ps <- partSymbols
      (nrow, ncol) <- neighborhood(ps.row, ps.col)
      found <- partNumbers.get((nrow, ncol))
    do
      val pn = (found.row, found.startcol, found.value)
      partMap = partMap.updatedWith((ps.row, ps.col, ps.symbol)) {
        case None => Some(Set(pn))
        case Some(s) => Some(s + pn)
      }
    partMap

  def part1(input:Iterator[String]): Int = {
    createPartMap(input)
      .values
      .flatten
      .map(_._3)
      .sum
  }

  def part2(input:Iterator[String]): Int =
    createPartMap(input)
      .filter(_._1._3 == "*")
      .values
      .filter(_.size == 2)
      .map(_.map(_._3))
      .map(_.product)
      .sum

  val testCaseInput = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

  val testCases: Seq[TestCase[_]] = Seq(
    TestCase[Int](part1, 4361, testCaseInput),
    TestCase[Int](part2, 467835, testCaseInput)
  )

  @main
  def runDay3Tests(): Unit = testCases.foreach(_.run())

  @main
  def runDay3Input(): Unit =
    println(s"Day 3 Part 1: ${part1(Source.fromResource("day3input.txt").getLines())}")
    println(s"Day 3 Part 1: ${part2(Source.fromResource("day3input.txt").getLines())}")

