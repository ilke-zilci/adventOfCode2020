package com.advent.day6

import scala.io.Source

object CustomsDeclaration {

  lazy val commonAnswers: Array[String] => Int =
    _.map(group => {
      val persons = group.split("\n")
      val answers = persons.map(_.toCharArray.toSet)
      answers.reduce { (a, b) =>
        a.intersect(b)
      }.size
    }).sum

  lazy val uniqueAnswers: Array[String] => Int =
    _.map(group => group.replaceAll("\n", "").toCharArray)
      .map(_.toSet.size)
      .sum

  def getGroups = {
    Source.fromResource("day6input").getLines().mkString("\n").split("\n\n")
  }
}
