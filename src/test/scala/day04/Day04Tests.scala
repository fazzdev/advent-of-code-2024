package day04

import utility.TestSuite

import scala.annotation.tailrec
import scala.util.matching.Regex

class Day04Tests extends TestSuite {
  test("Part One") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val result = sumHorizontallyBundle(input) + sumVertically(input)
    Console.println(result) // result = 2560
  }

  test("Part Two") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val result = sumCrossXMas(input)
    Console.println(result) // result = 2560
  }


  private def sumHorizontally(input: String, patterns: List[String] = List("XMAS", "SAMX")): Int =
    patterns.map(pattern => pattern.r.findAllIn(input).length).sum

  private def sumHorizontallyBundle(input: Iterable[String], patterns: List[String] = List("XMAS", "SAMX")): Int =
    input.map(s => sumHorizontally(s, patterns)).sum

  def makeHorizontal(strings: Vector[String]): Vector[String] =
    strings.foldLeft(Vector[String]()) {
      (result, current) => result.zipAll(current, "", "").map(v => v._1 + v._2.toString)
    }

  def shiftInput(strings: Vector[String]): Vector[String] =
    strings.zipWithIndex.map((string, index) => (1 to index).map(_ => "-").mkString + string)

  private def sumVertically(input: Array[String]) = {
    val vertical = makeHorizontal(input.toVector)
    Console.println("Vertical: ")
    vertical.foreach(Console.println)

    val diagonal1 = makeHorizontal(shiftInput(input.toVector))
    Console.println("Diagonal 1: ")
    diagonal1.foreach(Console.println)

    val diagonal2 = makeHorizontal(shiftInput(input.reverse.toVector))
    Console.println("Diagonal 2: ")
    diagonal2.foreach(Console.println)

    sumHorizontallyBundle(vertical) + sumHorizontallyBundle(diagonal1) + sumHorizontallyBundle(diagonal2)
  }

  private def sumCrossXMas(input: Array[String]) = {
    def markMas(strings: Vector[String]): Vector[Boolean] =
      strings.map(s => sumHorizontally(s, List("MAS", "SAM")) > 0)

    def markCrossMas(strings: Vector[String]): Vector[(Boolean, Boolean)] = {
      // Assume Vector size 3
      val left = makeHorizontal(shiftInput(strings))
      val right = makeHorizontal(shiftInput(strings.reverse))
      markMas(left).zip(markMas(right))
    }

    @tailrec
    def splitInputToBundleOf3(strings: Vector[String], accumulator: Vector[Vector[String]] = Vector()): Vector[Vector[String]] = {
      val result = accumulator ++ Vector(strings.take(3))
      if(strings.length > 3)
        splitInputToBundleOf3(strings.drop(1), result)
      else
        result
    }

    splitInputToBundleOf3(input.toVector)
      .flatMap(markCrossMas)
      .count(v => v._1 && v._2) // only count the one that is valid left and right
  }
}
