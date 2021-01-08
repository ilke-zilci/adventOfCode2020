package com.advent.day4

import com.advent.day4.ValidatePassports._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class PassportValidationTest extends AnyFunSuite with Matchers {

  test("pid validation") {
    validPid(Map("pid" -> "000000001")) shouldBe true
    validPid(Map("pid" -> "0123456789")) shouldBe false
  }

  test("hcl validation") {
    validHcl(Map("hcl" -> "#123abc")) shouldBe true
    validHcl(Map("hcl" -> "#123abz")) shouldBe false
    validHcl(Map("hcl" -> "123abc")) shouldBe false
  }

  test("height validation") {
    validHgt(Map("hgt" -> "60in")) shouldBe true
    validHgt(Map("hgt" -> "190cm")) shouldBe true
    validHgt(Map("hgt" -> "190in")) shouldBe false
    validHgt(Map("hgt" -> "190")) shouldBe false

  }
  test("valid pass method one") {
    validPartOne(
      "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
    ) shouldBe true

    validPartOne(
      "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
    ) shouldBe false
  }

  test("valid pass method two") {
    "iyr:2013\necl:brn hcl:#623a2f\ncid:246 byr:1948 pid:122719649\nhgt:160cm\neyr:2026"
      .replaceAll("\n", " ")
      .split(" ")
      .length shouldBe 8
  }

  test("recognize passports which have all mandatory fields") {
    val lines = Source.fromResource("day4input").getLines()
    countValidPasswords(lines, validPartOne) shouldBe 208
  }

  test("recognize passports which have all mandatory fields 2") {
    val lines = Source.fromResource("day4input").getLines()
    countValidPasswords(lines, validPartTwo) shouldBe 167
  }
}
