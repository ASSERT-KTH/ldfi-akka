package ldfi.akka.parser

import ldfi.akka.booleanformulas._
import scala.collection.mutable.ListBuffer
import scala.io.Source

object AkkaParser {

  object Clock {
    private var time: Int = 0

    def tick(): Unit = time = time + 1
    def getTime: Int = time
    def reset(): Unit = time = 0
  }

  def parse(input: Source,
            freePassMsgs: List[String]): FormattedLogs = {
    Clock.reset()
    var formattedLogs = ListBuffer[Row]()
    var previousSender, previousRecipient = ""
    val lines =
      input.getLines.filter(x => x.contains("received handled message"))

    for (line <- lines) {
      val filteredLine = line.replaceAll("\\s", "")
      val (currentSender, currentRecipient, currentMessage) =
        (parseSender(filteredLine),
         parseRecipient(filteredLine),
         parseMessage(line))
      val isNotFreePass = freePassMsgs.forall(freePassMessage =>
        !currentMessage.contains(freePassMessage))

      //only add messages that are part of the analysis
      if (isNotFreePass) {
        Clock.tick()
        val time = Clock.getTime
        previousSender = currentSender
        previousRecipient = currentRecipient
        formattedLogs += Row(currentSender,
                             currentRecipient,
                             time,
                             currentMessage)
      }
    }
    val format = FormattedLogs(formattedLogs.toList)
    format
  }

  def parseSender(line: String): String = {
    val pattern = """(?<=fromActor\[)(.+)(?=])""".r
    var sender = ""
    pattern.findAllIn(line).matchData foreach { m =>
      sender = m.group(0)
    }
    sender = sender.split("/").last
    if (sender.contains("#")) {
      sender = sender.split("#").toList.head
    }
    sender
  }

  def parseRecipient(line: String): String = {
    val pattern = """(?<=akka:)(.+)(?=-received)""".r
    var recipient = ""
    pattern.findAllIn(line).matchData foreach { m =>
      recipient = m.group(0);
    }
    recipient.split("/").last
  }

  def parseMessage(line: String): String = {
    val pattern = """(?<=message)(.+)(?=from)""".r
    pattern.findFirstIn(line) match {
      case Some(message) =>
        message.trim
          .replaceAll("""(?m)\s+$""", "") //remove trailing white spaces
      case None =>
        sys.error("Error: AkkaParser.parseMessage could not match any message")
    }
  }

  def prettyPrintFormat(format: FormattedLogs): Unit = {
    println("----------------------")
    for (l <- format.rows) {
      print(
        "\"sender\": " + l.sender + ", \"recipient\": " + l.recipient + ", ")
      print("\"time\": " + l.time + "\n")
    }
    println("----------------------")
  }

  case class FormattedLogs(rows: List[Row])
  case class Row(sender: String, recipient: String, time: Int, message: String)

}
