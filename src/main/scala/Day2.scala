package us.jkk.aoc2023

object Day2:
  /*
  To get information, once a bag has been loaded with cubes, the Elf will reach into the bag,
  grab a handful of random cubes, show them to you, and then put them back in the bag. He'll
  do this a few times per game.

  You play several games and record the information from each game (your puzzle input). Each
  game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated
  list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

  For example, the record of a few games might look like this:

  Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
  In game 1, three sets of cubes are revealed from the bag (and then put back again).
  The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes;
  the third set is only 2 green cubes.

  The Elf would first like to know which games would have been possible if the bag contained
  only 12 red cubes, 13 green cubes, and 14 blue cubes?
   */

  case class Draw(red: Int, blue: Int, green: Int)

  case class Game(id: Int, draws: Seq[Draw])

  def parseDraw(s:String): Draw =
    val drawData =
      s.split(",")
        .map(_.trim)
        .map {
          case s"${count} ${color}" =>
            color -> count.toInt
          }
        .toMap
    Draw(drawData.getOrElse("red", 0), drawData.getOrElse("blue", 0), drawData.getOrElse("green", 0))


  def parseInput(input:Iterator[String]): Iterator[Game] =
    input.flatMap {
      case s"Game ${idText}: ${allDrawsText}" =>
        val draws = allDrawsText.split(";").map(parseDraw)
        Some(Game(idText.toInt, draws))
      case _ =>
        None
    }

  def part1(input: Iterator[String]): Int =
    val validGameIds =
      parseInput(input)
        .filter(_.draws.forall(d => d.red <= 12 && d.green <= 13 && d.blue <= 14))
        .map(_.id)
    validGameIds.sum


  /*
  --- Part Two ---
The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!

As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
Game 4 required at least 14 red, 3 green, and 15 blue cubes.
Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.

For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?
   */

  def maxOf(game: Game, fn: Draw => Int): Int =
    game.draws.map(fn).maxOption.getOrElse(0)

  def part2(input: Iterator[String]): Int =
    val gamePowers =
      parseInput(input)
        .map(g => {
          val redMax = maxOf(g, _.red)
          val blueMax = maxOf(g, _.blue)
          val greenMax = maxOf(g, _.green)
          Seq(redMax, blueMax, greenMax).product
        })
    gamePowers.sum


  val testCaseInput = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

  val testCases: Seq[TestCase] = Seq(
    TestCase(part1, 8, testCaseInput),
    TestCase(part2, 2286, testCaseInput)
  )

  @main
  def runDay2Tests(): Unit = testCases.foreach(_.run())

  @main
  def runDay2Input(): Unit =
    println(s"Day 2 Part 1: ${part1(readInput(2))}")
    println(s"Day 2 Part 2: ${part2(readInput(2))}")

