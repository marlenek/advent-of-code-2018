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

  case class GuardActivity(id: Int, timeSpent: List[Int], activity: Activity)

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

  def main(args: Array[String]): Unit = {
    val guards = Source.fromResource("input.txt").getLines().toList.map(line => matchGuard(line))
    //  println(guards)

    val guards2 = guards.sortBy(_.time.getMillis)
    println(guards2.size)

    val guardsActivity = new ListBuffer[GuardActivity]()

    var id = -1
    for (i <- 0 until guards2.length - 1) {
      val curr = guards2(i)
      val next = guards2(i + 1)
      if (curr.id != -1)
        id = curr.id

      curr.activity match {
        case Awake => guardsActivity += GuardActivity(id, (curr.time.getMinuteOfHour to next.time.getMinuteOfHour - 1).toArray.toList, Awake)
        case Asleep => guardsActivity += GuardActivity(id, (curr.time.getMinuteOfHour to next.time.getMinuteOfHour - 1).toArray.toList, Asleep)
      }
    }

    println(guardsActivity.size)
    var sleepyhead = guardsActivity.toList.filter(_.activity == Asleep).groupBy(_.id).mapValues(_.flatMap(_.timeSpent).size).maxBy(_._2)._1
    var sleepyMinute = guardsActivity.toList.filter(_.activity == Asleep).groupBy(_.id).mapValues(_.flatMap(_.timeSpent)).filter(_._1 == sleepyhead).values.flatten.groupBy(identity).maxBy(_._2.size)._1

    var sleepyHead3 = guardsActivity.toList.filter(_.activity == Asleep).groupBy(_.id).mapValues(_.flatMap(_.timeSpent))
      .mapValues(_.groupBy(identity).mapValues(_.size)).maxBy(_._2.maxBy(_._2))

    var sleepyHead2 = guardsActivity.toList.filter(_.activity == Asleep).groupBy(_.id).mapValues(_.flatMap(_.timeSpent))
      .mapValues(_.groupBy(identity).mapValues(_.size)).maxBy(_._2.maxBy(_._2))._2.maxBy(_._2)._1 //.groupBy(_._2.identity).mapValues(_.size)
    // list.groupBy(identity).mapValues(_.size)
    println(sleepyHead2 + " " + sleepyHead3 + " " )//+ (sleepyHead2 * sleepyHead3))
   // println(sleepyHead2 * sleepyHead3)


  }
}

//112372 zle