package Parser

import java.io.BufferedReader

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.io.{BufferedSource, Source}
import BooleanFormulas._

object AkkaParser {


  def run(input: BufferedSource): FormattedLogs = {
    Clock.reset()
    val filename = "logs.log"
    var formattedLogs = ListBuffer[Row]()
    var previousSender, previousRecipient = ""
    val filteredLines = input.getLines.
      filter(x => x.contains("received") && !x.contains("deadLetters")).
      map(x => x.replaceAll("\\s", ""))

    for (line <- filteredLines) {
      val currentSender = parseSender(line)
      val currentRecipient = parseRecipient(line)
      //Only increase the clock in case there is a different sender & recipient.
      if (previousSender != currentSender && previousRecipient != currentRecipient) {
        Clock.tick()
      }
      val time = Clock.getTime
      previousSender = currentSender
      previousRecipient = currentRecipient
      formattedLogs += Row(currentSender, currentRecipient, time)
    }
    
    val format = FormattedLogs(formattedLogs.toList)

    format
  }

  def parseSender(line: String): String = {
    val pattern = """(?<=\[akka://system/user/)(.+)(?=#)""".r
    var sender = ""
    pattern.findAllIn(line).matchData foreach { m => sender = m.group(0) }
    sender
  }

  def parseRecipient(line: String): String = {
    val pattern = """(?<=akka://system/user/)(.+)(?=-received)""".r
    var recipient = ""
    pattern.findAllIn(line).matchData foreach { m => recipient = m.group(0); }
    recipient
  }

  object Clock {
    var time = 0
    def tick(): Unit = time = time + 1
    def getTime: String = time.toString
    def reset(): Unit = time = 0
  }

  def getAllNodes(format: FormattedLogs): HashSet[String] = {
    var dict = HashSet[String]()
    for (row <- format.rows) {
      dict += row.sender
      dict += row.recipient
    }
    dict
  }

  def prettyPrintFormat(format: FormattedLogs): Unit = {
    println("----------------------")
    for (l <- format.rows) {
      print("\"sender\": " + l.sender + ", \"recipient\": " + l.recipient + ", ")
      print("\"time\": " + l.time + "\n")
      /*
      print("\"time\": " + l.time.year + "-" + l.time.month + "-" + l.time.day + "::" +
      l.time.hour + ":" + l.time.minute + ":" + l.time.second + ":" + l.time.millisecond + "\n")
      */
    }
    println("----------------------")
  }


  case class FormattedLogs(rows: List[Row])
  case class Row(sender: String, recipient: String, time: String)
  case class Actor(name: String, id: Int)

}


//DATE
/*
  case class Date(year: String, month: String, day: String, hour: String, minute: String, second: String, millisecond: String)
  def parseDate(line: String): Date = {
    val pattern = raw"(\d{4})-(\d{2})-(\d{2})(\d{2}):(\d{2}):(\d{2}).(\d{3})".r
    var time = ""
    pattern.findAllIn(line).matchData foreach {
      m => time = m.group(0)
    }
    val date = time match {
      case pattern(year, month, day, hour, minute, second, millisecond) => Date(year, month, day, hour, minute, second, millisecond)
      case _ =>  Date("1", "1", "1", "1", "1", "1", "1") //no match
    }
    date
  }
 */



