import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by Marlena on 04.12.2018.
  */

object Guards {

  sealed trait Activity {}

  object Awake extends Activity

  object Asleep extends Activity

  case class Guard(time: DateTime, id: Int, activity: Activity)

  case class GuardActivity(id: Int, minutes: List[Int], activity: Activity)

  def matchGuard(line: String): Guard = {
    val formatter: DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
    val dateTimeRegex = """\[([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})\]"""
    val beginsShift = (dateTimeRegex + " Guard \\#(\\d+).*").r
    val fallsAsleep = (dateTimeRegex + " falls asleep").r
    val wakesUp = (dateTimeRegex + " wakes up").r

    line match {
      case beginsShift(d, t) => Guard(formatter.parseDateTime(d), t.toInt, Awake)
      case fallsAsleep(d) => Guard(formatter.parseDateTime(d), -1, Asleep)
      case wakesUp(d) => Guard(formatter.parseDateTime(d), -1, Awake)
    }
  }

  def sortGuardsByActivity(guards: List[Guard]): List[GuardActivity] = {
    val guardsSorted = guards.sortBy(_.time.getMillis)
    val guardsActivity = new ListBuffer[GuardActivity]()
    var id = -1
    for (i <- 0 until guardsSorted.length - 1) {
      val curr = guardsSorted(i)
      val next = guardsSorted(i + 1)
      if (curr.id != -1)
        id = curr.id
      guardsActivity += GuardActivity(id, (curr.time.getMinuteOfHour to next.time.getMinuteOfHour - 1).toArray.toList, curr.activity)
    }
    guardsActivity.toList
  }

  def getBestGuardAndMinute(guardsActivity: List[GuardActivity]): Int = {

    val sleepyGuards = guardsActivity.filter(_.activity == Asleep).groupBy(_.id)
    var sleepyHead = sleepyGuards.mapValues(_.flatMap(_.minutes).size).maxBy(_._2)._1
    var sleepyMinute = sleepyGuards.mapValues(_.flatMap(_.minutes)).filter(_._1 == sleepyHead).values.flatten.groupBy(identity).maxBy(_._2.size)._1

    sleepyHead * sleepyMinute
  }

  def getMostSleepyMinute(guardsActivity: List[GuardActivity]): Int = {

    var sleepyMinuteGuard = guardsActivity.filter(_.activity == Asleep).groupBy(_.id).mapValues(_.flatMap(_.minutes))
      .mapValues(_.groupBy(identity).mapValues(_.size)).mapValues(_.maxBy(_._2)).maxBy(_._2._2)

    sleepyMinuteGuard._1 * sleepyMinuteGuard._2._1
  }

  def main(args: Array[String]): Unit = {
    val guards = sortGuardsByActivity(Source.fromResource("input.txt").getLines().toList.map(line => matchGuard(line)))
    println("Part 1: " + getBestGuardAndMinute(guards))
    println("Part 2: " + getMostSleepyMinute(guards))
  }
}
