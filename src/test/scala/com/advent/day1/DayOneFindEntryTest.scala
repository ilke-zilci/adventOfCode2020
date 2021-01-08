package com.advent.day1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import com.advent.day1.DayOne._
import com.advent.day1.DayOnePartTwo.find2020InThreeCombs

class DayOneFindEntryTest extends AnyFunSuite with Matchers {
  test("find product of two numbers that sum to 2020") {
    val entries = getNumberIterator
    val result = find2020InTwoCombsStreaming(entries).toList.head
    result shouldBe 982464
  }

  test("find product of three numbers that sum to 2020") {
    val entries = getNumberIterator.toList
    val results = find2020InThreeCombs(entries)
    results should contain theSameElementsAs Set(162292410)
  }
}
