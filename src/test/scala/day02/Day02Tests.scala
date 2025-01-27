package day02

import utility.Extensions.*
import utility.TestSuite

class Day02Tests extends TestSuite {
  test("Part One") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val result = input.count(row => isSafe(row.split(" ")))
    Console.println(result) // result = 534
  }

  test("Part Two") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val result = input.count(row => isSafeWithDampener(row.split(" ")))
    Console.println(result) // result = 577
  }

  private def isSafe(strings: Array[String]): Boolean = {
    val diff = strings.zipDropOne().map((value1, value2) => value1.toInt - value2.toInt)
    if (diff.head < 0) diff.forall(v => -4 < v && v < 0)
    else diff.forall(v => 0 < v && v < 4)
  }

  private def isSafeWithDampener(strings: Array[String]): Boolean = {
    isSafe(strings) || (0 to strings.length).exists(index => isSafe(strings.remove(index)))
  }
}
