package main.scala

import scala.io.Source

object Checksum {

  def part1(boxIds: List[String]): Int = {
    var boxesWithTwoLetters, boxesWithThreeLetters = 0
    boxIds.foreach { b =>
      val boxMap = b.groupBy(_.toChar).mapValues(_.size)
      if (boxMap.exists(_._2 == 2)) boxesWithTwoLetters += 1
      if (boxMap.exists(_._2 == 3)) boxesWithThreeLetters += 1
    }
    boxesWithTwoLetters * boxesWithThreeLetters
  }

  def part2(boxIds: List[String]): String = {
    val nrOfBoxes = boxIds.length
    val lengthOfBox = boxIds.head.size

    for (i <- 0 to lengthOfBox - 1) {
      val newBoxes = boxIds.map(s => s.substring(0, i).concat(s.substring(i + 1, lengthOfBox)))
      if (newBoxes.toSet.size != nrOfBoxes)
        return newBoxes.diff(newBoxes.distinct).head
    }
    ""
  }

  def main(args: Array[String]): Unit = {
    val boxIds = Source.fromResource("input.txt").getLines().toList
    println("Result for part 1: " + part1(boxIds))
    println("Result for part 2: " + part2(boxIds))
  }
}