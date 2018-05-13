package ldfi.akka.Parser

import java.io.BufferedReader

import ldfi.akka.BooleanFormulas.BooleanFormula.Message

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.io.{BufferedSource, Source}
import ldfi.akka.BooleanFormulas._
import ldfi.akka.Controller.Controller

object AkkaParser {


  //TODO: If message is cut when rec and sender is the same, the clock doesn't notice it.
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
      val time = manageClock(currentSender, previousSender, currentRecipient, previousRecipient)
      previousSender = currentSender
      previousRecipient = currentRecipient
      formattedLogs += Row(currentSender, currentRecipient, time)
    }
    
    val format = FormattedLogs(formattedLogs.toList)
    format
  }


  def manageClock(curSen: String, prevSen: String, curRec: String, prevRec: String): Int = {

    if(curSen != prevSen){
      println("Before: " + Clock.getTime)
      Clock.tick()
      //Check for cuts
      while(shouldTick()){
        println("ShouldTick")
        Clock.tick()
      }
    }

    def shouldTick(): Boolean =  {
      val currentInjections = Controller.injections
      val curTime = Clock.getTime
      val currMsg = Message(curSen, curRec, curTime)
      val injectionsAtCurTime = currentInjections.collect { case msg @ Message(_, _, t) if t == curTime => msg }
      val sameSender = injectionsAtCurTime.exists(_.sender == curSen) && injectionsAtCurTime.nonEmpty

      val isInjected = injectionsAtCurTime.contains(currMsg)
      //The parser will not realize that some messages have been cut. This has to be corrected for when managing
      //the logical clock
      val res = injectionsAtCurTime.nonEmpty && (!sameSender | (sameSender && isInjected))
      println(sameSender + " " + injectionsAtCurTime + " " + res)
      res
    }
    Clock.getTime

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
    def tick(steps: Int): Unit = time = time + steps
    def getTime: Int = time
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
  case class Row(sender: String, recipient: String, time: Int)
  //case class Actor(name: String, id: Int)

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



