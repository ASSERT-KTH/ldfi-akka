package ldfi.akka.parser

import java.io.BufferedReader

import ldfi.akka.booleanformulas.BooleanFormula.{Literal, Message}
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.io.Source

object AkkaParser {

  object Clock {
    private var time : Int = 0

    def tick(): Unit = time = time + 1
    def getTime: Int = time
    def setTime(newTime: Int): Unit = time = newTime
    def reset(): Unit = time = 0
  }

  def parse(input: Source, injections: Set[Literal]): FormattedLogs = {
    Clock.reset()
    var formattedLogs = ListBuffer[Row]()
    var previousSender, previousRecipient =  ""
    val filteredLines = input.getLines.
      filter(x => x.contains("received") && !x.contains("deadLetters")).
      map(x => x.replaceAll("\\s", ""))

    for (line <- filteredLines) {
      val (currentSender, currentRecipient) = (parseSender(line), parseRecipient(line))
      val time = manageClock(currentSender, currentRecipient, previousSender, previousRecipient,
        Clock.getTime, injections)
      Clock.setTime(time)
      previousSender = currentSender
      previousRecipient = currentRecipient
      formattedLogs += Row(currentSender, currentRecipient, time)
    }

    val format = FormattedLogs(formattedLogs.toList)
    format
  }

  def manageClock(curSen: String, curRec: String, prevSen: String, prevRec: String,
                  curTime: Int, injections: Set[Literal]): Int = {
    //if new sender, increment clock
    if(curSen != prevSen)
      manageClockHelper(curSen, curRec, curTime + 1, injections)

    //same sender, but not all messages has been cut
    else if(curSen == prevSen && prevRec != curRec)
      manageClockHelper(curSen, curRec, curTime,  injections)

    //If same sender and recipient twice, then all messages have been cut in previous time step
    else if(curSen == prevSen && prevRec == curRec)
      manageClockHelper(curSen, curRec, curTime + 1,  injections)

    //default case, do not update clock
    else
      curTime
  }

  def manageClockHelper(curSen: String, curRec: String, curTime: Int, injections: Set[Literal]): Int = {
    if (shouldTick(curSen, curRec, curTime, injections))
      manageClockHelper(curSen, curRec, curTime + 1, injections)
    else
      curTime
  }

  def shouldTick(curSen: String, curRec: String, curTime: Int, injections: Set[Literal]) : Boolean = {
    val currMsg = Message(curSen, curRec, curTime)
    val injectionsAtCurTime = injections.collect { case msg @ Message(_, _, t) if t == curTime => msg }
    val sameSender = injectionsAtCurTime.exists(_.sender == curSen) && injectionsAtCurTime.nonEmpty
    val isInjected = injectionsAtCurTime.contains(currMsg)

    //The parser will not realize that some messages have been cut. This has to be corrected for when managing
    //the logical clock
    val res = injectionsAtCurTime.nonEmpty && (!sameSender | (sameSender && isInjected))
    res
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
    }
    println("----------------------")
  }

  case class FormattedLogs(rows: List[Row])
  case class Row(sender: String, recipient: String, time: Int)

}






