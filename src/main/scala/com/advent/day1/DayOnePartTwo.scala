package com.advent.day1

object DayOnePartTwo {
  def find2020InThreeCombs(entries: List[Int]): Set[Int] = {
    val combs = getThreeCombs(entries)
    combs
      .filter { case (x, y, z) => x + y + z == 2020 }
      .map { case (x, y, z) => x * y * z }
  }

  private def getThreeCombs(entries: List[Int]): Set[(Int, Int, Int)] = {
    val combs = for {
      x <- entries
      y <- entries
      z <- entries
    } yield (x, y, z)
    val uniqueCombs = combs.toSet
    uniqueCombs.filterNot {
      case (x, y, z) => ((x == y) || (x == z) || (y == z))
    }
  }
}
