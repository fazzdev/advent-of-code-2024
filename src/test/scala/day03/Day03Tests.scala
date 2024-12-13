package day03

import utility.TestSuite

import scala.util.matching.Regex

class Day03Tests extends TestSuite {
  test("Part One") {
    //val input = resourceAsString("SimpleInput.txt")
    val input = resourceAsString("Input.txt")
    val result = parseMul(input).map((v1, v2) => v1 * v2).sum
    Console.println(result) // result = 188192787
  }

  test("Part Two") {
    //val input = resourceAsString("SimpleInput.txt")
    val input = resourceAsString("Input.txt")
    val result = parseMul(removeDont(input)).map((v1, v2) => v1 * v2).sum
    Console.println(result) // result = 113965544
  }

  private def removeDont(input: String): String = {
    val splitByDo = input.split("do\\(\\)")
    val splitByDont =  splitByDo.flatMap(s => s.split("don't\\(\\)").take(1))

    splitByDont.mkString
  }

  private def parseMul(input: String): Iterator[(Int, Int)] = {
    val pattern: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r
    pattern.findAllMatchIn(input).map { m =>
      (m.group(1).toInt, m.group(2).toInt)
    }
  }
}
