package com.advent.day1

import scala.io.Source

case class FirstLevelAggregator(
    dropCount: Int,
    result: Option[Int],
    found: Boolean
)
object FirstLevelAggregator {
  def empty: FirstLevelAggregator = FirstLevelAggregator(1, None, false)
}

case class SecondLevelAggregator(secondNumber: Int, found: Boolean)
object SecondLevelAggregator {
  def empty: SecondLevelAggregator = SecondLevelAggregator(0, false)
}

object DayOne {
  def getNumberIterator = {
    Source.fromResource("day1input").getLines().map(_.toInt)
  }

  def find2020InTwoCombsStreaming(entries: Iterator[Int]) = {
    entries
      .scanLeft(FirstLevelAggregator.empty) {
        case (acc, x) if !acc.found =>
          findAndMultiply(
            x,
            getNumberIterator.drop(acc.dropCount)
          ).toList match {
            case Nil => FirstLevelAggregator(acc.dropCount + 1, None, false)
            case resultList =>
              FirstLevelAggregator(acc.dropCount, resultList.headOption, true)
          }
        case (acc, _) if acc.found => acc
      }
      .collect { case FirstLevelAggregator(_, Some(res), _) => res }
  }

  private def findAndMultiply(first: Int, entries: Iterator[Int]) = {
    entries
      .scanLeft(SecondLevelAggregator.empty) {
        case (_, x) if ((first + x) == 2020) =>
          SecondLevelAggregator(secondNumber = x, true)
        case (_, x) if ((first + x) != 2020) =>
          SecondLevelAggregator(0, false)
      }
      .collect {
        case SecondLevelAggregator(secondNumber, found) if found =>
          first * secondNumber
      }
  }
}
