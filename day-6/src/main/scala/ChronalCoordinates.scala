import java.util

import scala.io.Source

/**
  * Created by Marlena on 06.12.2018.
  */
object ChronalCoordinates {

  case class Point(var x: Int, var y: Int)

  def manhattanDistance(p1: Point, p2: Point): Int = {
    (p1.x - p2.x).abs + (p1.y - p2.y).abs
  }

  def getPointWithLargestArea(points: List[Point]): Int = {
    val maxX = points.maxBy(_.x).x // 338
    val maxY = points.maxBy(_.y).y // 353
    val minX = points.minBy(_.x).x
    val minY = points.minBy(_.y).y

    var pointsWithLocations = collection.mutable.Map[Point, List[Point]]()

    var grid = new util.ArrayList[Point]

    for (i <- minX to maxX; j <- minY to maxY) {
      grid.add(Point(i, j))
    }

    grid.forEach(g => {
      var mapOfDistances = collection.mutable.Map[Point, Int]()
      points.foreach(p => mapOfDistances.put(p, manhattanDistance(p, g)))

      var shortestPoints = mapOfDistances.filter(p => p._2 == mapOfDistances.minBy(_._2)._2)
      if (shortestPoints.size == 1) {
        pointsWithLocations.get(shortestPoints.head._1) match {
          case Some(xs: List[String]) => pointsWithLocations.update(shortestPoints.head._1, xs :+ g)
          case None => pointsWithLocations.update(shortestPoints.head._1, List(g))
        }
      }
    })
    pointsWithLocations.retain((k, v) => !(v.contains(Point(k.x, maxY)) || v.contains(Point(maxX, k.y)) || v.contains(Point(k.x, minY)) || v.contains(Point(minX, k.y))))
    pointsWithLocations.groupBy(identity).maxBy(_._2.size)._2.values.flatten.size
  }

  def main(args: Array[String]): Unit = {

    var points = Source.fromResource("test_input.txt").getLines().map(line => Point(line.split(", ")(0).toInt, line.split(", ")(1).toInt)).toList
    println("Result for part 1: " + getPointWithLargestArea(points))
    //println("Result for part 2: ")
    // 1788 to low :( reddit says 2906
  }

}
