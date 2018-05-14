package Parser

import ldfi.akka.BooleanFormulas.BooleanFormula._
import ldfi.akka.Parser.AkkaParser
import ldfi.akka.Parser.AkkaParser.{FormattedLogs, Row}
import org.scalatest.FunSuite
import ldfi.akka.Controller.Controller

import scala.io.{BufferedSource, Source}

class AkkaParserSuite extends FunSuite {

  testparseSender()
  testparseRecipient()
  testAkkaParser()
  testmanageClock()


  def testAkkaParser(): Unit = {
    val input: BufferedSource = Source.fromFile("src/test/scala/Parser/testLogs.log")
    val res: FormattedLogs = AkkaParser.run(input)
    test("Testing AkkaParser") {
      assert(res == FormattedLogs(List(Row("A", "B", 1), Row("A", "C", 1))))
    }

  }


  def testmanageClock(): Unit = {

    val inj1 = Set(Message("A", "C", 1).asInstanceOf[Literal])
    test("Assert that time does not tick when same injection sender and not injected") {
      assert(AkkaParser.manageClock("A", "B", "", 0, inj1) == 1)
    }

    val inj2 = Set(Message("B", "A", 1).asInstanceOf[Literal])
    test("Assert that time ticks when not same injection sender") {
      assert(AkkaParser.manageClock("A", "B", "", 0, inj2) == 2)
    }

    val inj3 = Set(Message("A", "B", 1).asInstanceOf[Literal])
    test("Assert that time ticks when same injection sender and injected") {
      assert(AkkaParser.manageClock("A", "B", "", 0, inj3) == 2)

    }


  }

  def testparseSender(): Unit = {
    val line = "DEBUG[system-akka.actor.default-dispatcher-3]akka://system/user/C-receivedhandledmessageBroadcast(Somepayload)fromActor[akka://system/user/A#-1746850710]"

    val sender: String = AkkaParser.parseSender(line)
    test("testing parseSender") {
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
