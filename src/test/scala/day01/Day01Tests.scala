package day01

import utility.TestSuite

class Day01Tests extends TestSuite {
  test("Part One") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val bucket = input.foldLeft(Bucket())((bucket, current) => bucket.add(current.split(" {3}"))).sort()
    val result = bucket.left
      .zip(bucket.right)
      .foldLeft(0)((count, value) => count + Math.abs(value._1 - value._2))
    Console.println(result) // result = 1579939
  }

  test("Part Two") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val bucket = input.foldLeft(Bucket())((bucket, current) => bucket.add(current.split(" {3}")))
    val result = bucket.left
      .map(leftValue => leftValue * bucket.right.count(_ == leftValue))
      .sum
    Console.println(result) // result = 20351745
  }
}

object Bucket {
  def apply(): Bucket = new Bucket(List.empty, List.empty)
}

case class Bucket(left: List[Int], right: List[Int]) {
  def add(strings: Array[String]): Bucket = add(strings(0).toInt, strings(1).toInt)
  def add(leftItem: Int, rightItem: Int): Bucket = Bucket(leftItem +: left, rightItem +: right)
  def sort(): Bucket = Bucket(left.sorted, right.sorted)
}
