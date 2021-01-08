package com.advent.day5

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import com.advent.day5.FindSeats._

class HighestSeatTest extends AnyFunSuite with Matchers {
  test("find range with char") {
    narrowDown('F', InclusiveRange(1, 128)) shouldBe InclusiveRange(1, 64)
    narrowDown('B', InclusiveRange(1, 64)) shouldBe InclusiveRange(33, 64)
    narrowDown('F', InclusiveRange(33, 64)) shouldBe InclusiveRange(33, 48)
  }

  test("find row in 7 chars") {
    val str = "FBFBBFF"
    val instructions = str.toList
    binSearchInRange(instructions, InclusiveRange(1, 128)) shouldBe 44
  }

  test("find column in 3 chars") {
    binSearchInRange("RLR".toList, InclusiveRange(1, 8)) shouldBe 5
  }

  test("calculate seat Id") {
    val row = 44
    val col = 5
    seat(row, col) shouldBe 357
  }

  test("seat id") {
    getSeatId("FBFBBFFRLR") shouldBe 357
  }

  test("get seat id of each line and max") {
    findMaxSeatId shouldBe 885
  }

  test("find full list of seat Ids") {
    val result = findOwnSeatId
    result should contain theSameElementsAs List(623)
  }
}
