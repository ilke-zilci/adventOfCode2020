package com.advent.day6

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.advent.day6.CustomsDeclaration._

class CustomsDeclarationTest extends AnyFunSuite with Matchers {
  test("should sum unique answers in each group") {
    uniqueAnswers(getGroups) shouldBe 6686
  }

  test("should sum common answers in each group") {
    commonAnswers(getGroups) shouldBe 3476
  }
}
