package com.advent.day2

import scala.util.Try

object ValidatePasswords {
  case class Password(pw: String)
  case class Policy(char: Char, from: Int, to: Int)

  def parse(str: String) = {
    val parts = str.split(":").map(_.trim)
    (parts(0), Password(parts(1)))
  }

  private def stringToPolicy(policy: String) = {
    val policyParts = policy.split("\\s")
    val char = policyParts(1)
    val fromTo = policyParts(0).split("-")
    Policy(char.charAt(0), fromTo(0).toInt, fromTo(1).toInt)
  }

  def validatePartOne(policy: Policy, pw: Password): Boolean = {
    val occurence = pw.pw.toCharArray.count(policy.char == _)
    (policy.from <= occurence) && (occurence <= policy.to)
  }

  def validatePartTwo(policy: Policy, pw: Password): Boolean = {
    Try {
      (pw.pw.charAt(policy.from - 1) == policy.char) ^
        (pw.pw.charAt(policy.to - 1) == policy.char)
    }.toOption.getOrElse(false)
  }

  def countValidPws(
      entries: Iterator[String],
      validate: (Policy, Password) => Boolean
  ): Int = {
    entries.toList
      .map(parse)
      .map {
        case (policy, pw) => (stringToPolicy(policy), pw)
      }
      .count {
        case (policy, pw) => validate(policy, pw)
      }
  }
}
