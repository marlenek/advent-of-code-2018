package main.scala
import scala.collection.mutable
import scala.io.Source

/**
  * Created by Marlena on 04.12.2018.
  */

object AlchemicalReduction {

  def calculateUnitsLeft(units: List[Char]): Int = {
    var stack = mutable.Stack[Char]()
    units.foreach(c =>
      if (stack.nonEmpty) {
        if (stack.top.toUpper.equals(c.toUpper) && (stack.top.toInt - c.toInt).abs == 32) {
          stack.pop()
        }
        else stack.push(c)
      }
      else stack.push(c)
    )
    stack.size
  }

  def calculateShortestPolymer(units: List[Char]): Int = {
    ('a' to 'z').map(c => calculateUnitsLeft(units.filterNot(char => char.equals(c) || char.equals(c.toUpper)))).min
  }

  def main(args: Array[String]): Unit = {
    var units = Source.fromResource("input.txt").getLines().toList.flatten
    println("Result for part 1: " + calculateUnitsLeft(units))
    println("Result for part 2: " + calculateShortestPolymer(units))
  }
}