package com.advent.day3

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import com.advent.day3.CountTrees._

class CountTreesTest extends AnyFunSuite with Matchers {

  test("in mini pattern find trees in right 3 down 1 slope to reach bottom") {
    val pattern =
      Source.fromResource("day3input").getLines()
    val slope = (3, 1)
    val count = countTrees(pattern, slope)
    count.numberOfTrees shouldBe 159
  }

  test("in mini pattern find trees in 1,1 slope to reach bottom") {
    val pattern =
      Source.fromResource("day3input").getLines()
    val slope = (1, 1)
    val count = countTrees(pattern, slope)
    count.numberOfTrees shouldBe 86
  }

  test("in mini pattern find trees in 3,1 slope to reach bottom") {
    val pattern =
      Source.fromResource("day3input").getLines()
    val slope = (3, 1)
    val count = countTrees(pattern, slope)
    count.numberOfTrees shouldBe 159
  }

  test("in mini pattern find trees in 5,1 slope to reach bottom") {
    val pattern =
      Source.fromResource("day3input").getLines()
    val slope = (5, 1)
    val count = countTrees(pattern, slope)
    count.numberOfTrees shouldBe 97
  }

  test("in mini pattern find trees in 7,1 slope to reach bottom") {
    val pattern =
      Source.fromResource("day3input").getLines()
    val slope = (7, 1)
    val count = countTrees(pattern, slope)
    count.numberOfTrees shouldBe 88
  }

  test("in mini pattern find trees in 1,2 slope to reach bottom") {
    val pattern =
      Source.fromResource("day3input").getLines()
    val slope = (1, 2)
    val count = countTrees(pattern, slope)
    count.numberOfTrees shouldBe 55
  }
}
