package com.advent.day5

import scala.annotation.tailrec
import scala.io.Source

object FindSeats {
  case class InclusiveRange(start: Int, end: Int)

  @tailrec
  final def binSearchInRange(
      instructions: List[Char],
      range: InclusiveRange
  ): Int = {
    instructions match {
      case Nil => range.end - 1
      case x :: xs =>
        val newRange = narrowDown(x, range)
        binSearchInRange(xs, newRange)
    }
  }

  def narrowDown(char: Char, range: InclusiveRange): InclusiveRange = {
    val mid = range.start + (range.end - range.start) / 2
    char match {
      case 'F' | 'L' =>
        InclusiveRange(range.start, mid)
      case 'B' | 'R' =>
        InclusiveRange(mid + 1, range.end)
    }
  }

  def seat(row: Int, col: Int) = row * 8 + col

  def getSeatRowCol: String => (Int, Int) = { l =>
    val instructions = l.toList
    val rowInfo = instructions.slice(0, 7)
    val colInfo = instructions.slice(7, 10)
    val row = binSearchInRange(rowInfo, InclusiveRange(1, 128))
    val col = binSearchInRange(colInfo, InclusiveRange(1, 8))
    (row, col)
  }

  def getSeatId(str: String): Int = {
    val (r, c) = getSeatRowCol(str)
    seat(r, c)
  }

  def findMaxSeatId: Int = {
    getAvailableSeatRowCols.map {
      case (r, c) => seat(r, c)
    }.max
  }

  private def getAvailableSeatRowCols = {
    Source.fromResource("day5input").getLines().map(getSeatRowCol)
  }

  def findOwnSeatId = {
    val rows: Seq[Int] = Range(0, 128).toList
    val columns: Seq[Int] = Range(0, 8).toList

    val allSeats = for {
      r <- rows
      c <- columns
    } yield (r, c)
    val available = getAvailableSeatRowCols.toSeq

    val missing = allSeats.filterNot {
      case (x, y) =>
        available.contains((x, y))
    }

    val availableSeatIds = available.map {
      case (r, c) => seat(r, c)
    }

    val missingSeatIds: Seq[Int] = missing.map { case (r, c) => seat(r, c) }
    missingSeatIds.filter { s =>
      availableSeatIds.contains(s - 1) && availableSeatIds.contains(s + 1)
    }
  }

}
