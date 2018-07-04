package ldfi.akka.parser

import ldfi.akka.booleanformulas._
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

  def parse(input: Source, injections: Set[Literal], freePassMsgs: List[String]): FormattedLogs = {
    Clock.reset()
    var formattedLogs = ListBuffer[Row]()
    var previousSender, previousRecipient =  ""
    val lines = input.getLines
      .filter(x => x.contains("received handled message"))

    for (line <- lines) {
      val filteredLine = line.replaceAll("\\s", "")
      val (currentSender, currentRecipient, currentMessage) =
        (parseSender(filteredLine), parseRecipient(filteredLine), parseMessage(line))
      val isNotFreePass = freePassMsgs.forall(freePassMessage => !currentMessage.contains(freePassMessage))

      //only add messages that are part of the analysis
      if(isNotFreePass){
        val time = manageClock(currentSender, currentRecipient, previousSender, previousRecipient, Clock.getTime,
          currentMessage, injections.toList)
        Clock.setTime(time)
        previousSender = currentSender
        previousRecipient = currentRecipient
        formattedLogs += Row(currentSender, currentRecipient, time, currentMessage)
      }
    }
    val format = FormattedLogs(formattedLogs.toList)
    format
  }

  def manageClock(curSen: String, curRec: String, prevSen: String, prevRec: String,
                  curTime: Int, curMsg: String, injections: List[Literal]): Int = {
    //if new sender, increment clock
    if(curSen != prevSen){
      manageClockHelper(curSen, curRec, curTime + 1, curMsg, injections)
    }

    //same sender, but not all messages has been cut
    else if(curSen == prevSen && prevRec != curRec){
      manageClockHelper(curSen, curRec, curTime, curMsg, injections)
    }

    //If same sender and recipient twice, then all messages have been cut in previous time step
    else if(curSen == prevSen && prevRec == curRec){
      val existsNextInjection = injections.collect {
        case msg @ MessageLit(sen, _, t, _) if t == curTime + 1 && curSen != sen=>
          msg
      }.nonEmpty
      //if there exists an injection at next step with a different actor,
      // then we know that a different actor sent messages in between this message and the last one (but they were omitted)
      if(existsNextInjection){
        manageClockHelper(curSen, curRec, curTime + 1, curMsg, injections)
      }
      else {
        curTime
      }
    }

    //default case, do not update clock
    else {
      curTime
    }
  }

  def manageClockHelper(curSen: String, curRec: String, curTime: Int, curMsg: String, injections: List[Literal]): Int = {
    if (shouldTick(curSen, curRec, curTime, curMsg, injections)){
      manageClockHelper(curSen, curRec, curTime + 1, curMsg, injections)
    }
    else {
      curTime
    }
  }

  def shouldTick(curSen: String, curRec: String, curTime: Int, curMsg: String, injections: List[Literal]) : Boolean = {
    val curMsgLit = MessageLit(curSen, curRec, curTime, curMsg)
    val injectionsAtCurTime = injections.collect { case msg @ MessageLit(_, _, t, _) if t == curTime => msg }
    //check if there are messages that has been injected by this sender at this time
    val sameInjectionSender = injectionsAtCurTime.exists(_.sender == curSen)
    val isInjected = injectionsAtCurTime.contains(curMsgLit)

    //The parser will not realize that some messages have been cut. This has to be corrected for when managing
    //the logical clock
    val res = injectionsAtCurTime.nonEmpty && (!sameInjectionSender | (sameInjectionSender && isInjected))
    res
  }

  def parseSender(line: String): String = {
    val pattern = """(?<=fromActor\[)(.+)(?=])""".r
    var sender = ""
    pattern.findAllIn(line).matchData foreach { m => sender = m.group(0) }
    sender = sender.split("/").last
    if(sender.contains("#")){
      sender = sender.split("#").toList.head
    }
    sender
  }

  def parseRecipient(line: String): String = {
    val pattern = """(?<=akka:)(.+)(?=-received)""".r
    var recipient = ""
    pattern.findAllIn(line).matchData foreach { m => recipient = m.group(0); }
    recipient.split("/").last
  }

  def parseMessage(line: String): String = {
    val pattern = """(?<=message)(.+)(?=from)""".r
    pattern.findFirstIn(line) match {
      case Some(message) => message.trim.replaceAll("""(?m)\s+$""", "") //remove trailing white spaces
      case None => sys.error("Error: AkkaParser.parseMessage could not match any message")
    }
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
  case class Row(sender: String, recipient: String, time: Int, message: String)

}