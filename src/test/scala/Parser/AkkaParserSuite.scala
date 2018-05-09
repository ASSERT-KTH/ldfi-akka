package Parser

import ldfi.akka.Parser.AkkaParser
import ldfi.akka.Parser.AkkaParser.{FormattedLogs, Row}
import org.scalatest.FunSuite

import scala.io.{BufferedSource, Source}

class AkkaParserSuite extends FunSuite {

  testAkkaParser()
  testparseSender()
  testparseRecipient()

  def testAkkaParser(): Unit = {
    val input: BufferedSource = Source.fromFile("src/test/scala/Parser/testLogs.log")
    val res: FormattedLogs = AkkaParser.run(input)
    test("Testing AkkaParser") {
      assert(res == FormattedLogs(List(Row("A", "B", "1"), Row("A", "C", "1"))))
    }
  }

  def testparseSender(): Unit = {
    val line = "DEBUG[system-akka.actor.default-dispatcher-3]akka://system/user/C-receivedhandledmessageBroadcast(Somepayload)fromActor[akka://system/user/A#-1746850710]"

    val sender: String = AkkaParser.parseSender(line)
    test("testing parseSender"){
      assert(sender == "A")
    }

  }

  def testparseRecipient(): Unit = {
    val line = "DEBUG[system-akka.actor.default-dispatcher-3]akka://system/user/C-receivedhandledmessageBroadcast(Somepayload)fromActor[akka://system/user/A#-1746850710]"
    val recipient: String = AkkaParser.parseRecipient(line)
    test("testing parseRecipient") {
      assert(recipient == "C")
    }
  }




}
