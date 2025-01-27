package day05

import utility.Extensions.*
import utility.TestSuite

import scala.annotation.tailrec

class Day05Tests extends TestSuite {
  test("Part One") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val result = countMiddlePointForInOrder(input)
    Console.println(result) // result = 6384
  }

  test("Part Two") {
    //val input = resourceAsString("SimpleInput.txt").split("\n")
    val input = resourceAsString("Input.txt").split("\n")
    val result = countMiddlePointForNotInOrder(input)
    Console.println(result) // result = 5353
  }

  private def isOrdered(pages: Array[String], orderRules: Array[String]) = {
    val combinedPages = pages.zipDropOne().map((a, b) => s"$a|$b")

    combinedPages.forall(p => orderRules.contains(p))
  }

  private def countMiddlePointForInOrder(input: Array[String]): Int = {
    val splitInput = input.indexOf("")
    val (orderRules, pageInput) = (input.take(splitInput), input.drop(splitInput + 1))

    val correctlyOrderedPageInput = pageInput
      .map(p => p.split(","))
      .filter(p => isOrdered(p, orderRules))

    correctlyOrderedPageInput.map(p => p.middle().toInt).sum
  }

  private def countMiddlePointForNotInOrder(input: Array[String]): Int = {
    val splitInput = input.indexOf("")
    val (orderRules, pageInput) = (input.take(splitInput), input.drop(splitInput + 1))

    val wronglyOrderedPageInput = pageInput
      .map(p => p.split(","))
      .filter(p => !isOrdered(p, orderRules))

    val reorderedPageInput =
      wronglyOrderedPageInput
        .map(p => sortPages(p, orderRules))

    reorderedPageInput.map(p => p.middle().toInt).sum
  }

  private def sortPages(pages: Array[String], orderRules: Array[String]): Array[String] = {
    @tailrec
    def sortPagesWork(pages: Array[String], orderRules: Array[String], index: Int = 0): Array[String] = {
      if (index >= pages.length)
        pages
      else {
        val current = pages(index)
        val pagesToCheck = pages.zipWithIndex.drop(index + 1)

        val firstMismatch = pagesToCheck.collectFirst { case (p, pIndex) if orderRules.contains(s"$p|$current") =>
          pIndex
        }

        firstMismatch match {
          case Some(pIndex) => sortPagesWork(pages.swap(index, pIndex), orderRules, index)
          case None => sortPagesWork(pages, orderRules, index + 1)
        }
      }
    }

    sortPagesWork(pages, orderRules)
  }
}
