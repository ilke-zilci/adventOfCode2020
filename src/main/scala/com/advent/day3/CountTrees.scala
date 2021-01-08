package com.advent.day3

object CountTrees {
  case class TreeAggregator(positionX: Int, positionY: Int, numberOfTrees: Int)
  object TreeAggregator {
    def empty: TreeAggregator = TreeAggregator(0, 0, 0)
  }

  def countTrees(
      areaMap: Iterator[String],
      slope: (Int, Int)
  ): TreeAggregator = {
    val (step, down) = slope
    areaMap.foldLeft(TreeAggregator.empty) {
      case (acc, x) =>
        val currentX = acc.positionX
        val currentY = acc.positionY
        val isDownMoveDone = (currentY % down) == 0
        val (newX, trees) = (isDownMoveDone, isTree(x, currentX)) match {
          case (true, true) =>
            (currentX + step, acc.numberOfTrees + 1)
          case (true, false) =>
            (currentX + step, acc.numberOfTrees)
          case (false, _) =>
            (currentX, acc.numberOfTrees)
        }

        TreeAggregator(
          positionX = newX,
          positionY = acc.positionY + 1,
          numberOfTrees = trees
        )
    }
  }

  //open squares (.) and trees (#)
  private def isTree(pattern: String, position: Int): Boolean = {
    val array = pattern.toCharArray
    val patternLength = array.size
    val path = (position % patternLength)
    val c = array(path)
    c.equals('#')
  }
}
