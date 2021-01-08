package com.advent.day2

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import com.advent.day2.ValidatePasswords._

class FindValidPasswordTest extends AnyFunSuite with Matchers {
  test("parse password and policy from line") {
    val pwAndPolicy = parse("1-3 a: abcde")
    pwAndPolicy._1 shouldBe "1-3 a"
    pwAndPolicy._2.pw shouldBe "abcde"
  }

  test("count valid pws part one") {
    val entries = Source.fromResource("day2input").getLines()
    val count = countValidPws(entries, validatePartOne)
    count shouldBe 378
  }

  test("validation second method") {
    validatePartTwo(Policy('a', 1, 3), Password("abcde")) shouldBe true
    validatePartTwo(Policy('b', 1, 3), Password("cdefg")) shouldBe false
    validatePartTwo(Policy('c', 2, 9), Password("ccccccccc")) shouldBe false
  }

  test("count valid pws part two") {
    val entries = Source.fromResource("day2input").getLines()
    val count = countValidPws(entries, validatePartTwo)
    count shouldBe 280
  }
}
