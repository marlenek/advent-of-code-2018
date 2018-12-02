package main.scala

import scala.io.Source

object Checksum {

  def part1(boxesList: List[String]): Int = {
    var boxesWithTwoLetters, boxesWithThreeLetters = 0
    boxesList.foreach { b =>
      val boxMap = b.groupBy(_.toChar).mapValues(_.size)
      if (boxMap.exists(_._2 == 2)) boxesWithTwoLetters += 1
      if (boxMap.exists(_._2 == 3)) boxesWithThreeLetters += 1
    }
    boxesWithTwoLetters * boxesWithThreeLetters
  }

  def part2(boxesList: List[String]): String = {
    val boxLength = boxesList.head.size
    for (i <- 0 to boxLength - 1) {
      val boxesWithoutOneLetter = boxesList.map(s => s.substring(0, i).concat(s.substring(i + 1, boxLength)))
      if (boxesWithoutOneLetter.toSet.size != boxesList.length)
        return boxesWithoutOneLetter.diff(boxesWithoutOneLetter.distinct).head
    }
    ""
  }

  def main(args: Array[String]): Unit = {
    val boxesList = Source.fromResource("input.txt").getLines().toList
    println("Result for part 1: " + part1(boxesList))
    println("Result for part 2: " + part2(boxesList))
  }
}