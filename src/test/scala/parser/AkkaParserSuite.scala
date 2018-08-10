package parser

import ldfi.akka.booleanformulas._
import ldfi.akka.parser.AkkaParser
import ldfi.akka.parser.AkkaParser.{FormattedLogs, Row}
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class AkkaParserSuite extends FunSuite with Matchers {

  testparseSender()
  testparseRecipient()
  testAkkaParser()
  testmanageClock()

  def testAkkaParser(): Unit = {
    val logs = Source.fromFile("src/test/scala/parser/testLogs.log").mkString.split("\n\n")

    //B receives from A, C receives from A
    logs.lift(0) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set.empty, List("Start"))
        test("Testing AkkaParser, two messages sent with no injections") {
          assert(res == FormattedLogs(List(Row("A", "B", 1, "Broadcast(Some payload)"),
            Row("A", "C", 1, "Broadcast(Some payload)"))))
        }
      case None => println("testLogs are empty at position " + 0)
    }

    //B receives from A
    logs.lift(1) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(MessageLit("A", "C", 1, "Broadcast(Some payload)")), List("Start"))
        test("Testing AkkaParser, two messages sent with single injection at time 1") {
          assert(res == FormattedLogs(List(Row("A", "B", 1, "Broadcast(Some payload)"))))
        }
      case None => println("testLogs are empty at position " + 1)
    }

    //B receives from A
    logs.lift(1) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val injections =
          List(
            MessageLit("A", "B", 1, "Broadcast(Some payload)"),
            MessageLit("B", "A", 2, "Broadcast(Some payload)"))
        val res: FormattedLogs = AkkaParser.parse(src, injections.toSet, List("Start"))
        test("Testing AkkaParser, one messages sent with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "B", 3, "Broadcast(Some payload)"))))
        }
      case None => println("testLogs are empty at position " + 1)
    }

    //C receives from A, C receives from A
    logs.lift(2) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set.empty, List("Start"))
        test("Testing AkkaParser, two same messages sent to same actor with no injections") {
          val rows = List(Row("A", "C", 1, "Broadcast(Some payload)"), Row("A", "C", 2, "Broadcast(Some payload)"))
          assert(res == FormattedLogs(rows))
        }
      case None => println("testLogs are empty at position " + 2)
    }

    //C receives from A, C receives from A
    logs.lift(2) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val injections =
          List(
            MessageLit("A", "B", 1, "Broadcast(Some payload)"),
            MessageLit("B", "A", 2, "Broadcast(Some payload)"))
        val res: FormattedLogs = AkkaParser.parse(src, injections.toSet, List("Start"))
        test("Testing AkkaParser, two same messages sent with injection at first step & \"ghost\" time at step 2") {
          val rows = List(Row("A", "C", 1, "Broadcast(Some payload)"), Row("A", "C", 3, "Broadcast(Some payload)"))
          assert(res == FormattedLogs(rows))
        }
      case None => println("testLogs are empty at position " + 2)
    }

    //C receives from A, B receives from A
    logs.lift(3) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(MessageLit("A", "B", 1, "Broadcast(Some payload)"),
          MessageLit("B", "A", 2, "Broadcast(Some payload)")), List("Start"))
        test("Testing AkkaParser, two diff messages sent with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1, "Broadcast(Some payload)"),
            Row("A", "B", 3, "Broadcast(Some payload)"))))
        }
      case None => println("testLogs are empty at position " + 3)
    }

    //C receives from A, B receives from C
    logs.lift(4) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(MessageLit("A", "B", 1, "Broadcast(Some payload)"),
          MessageLit("B", "A", 2, "Broadcast(Some payload)")), List("Start"))
        test("Testing AkkaParser, different senders sent with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1, "Broadcast(Some payload)"),
            Row("C", "B", 3, "Broadcast(Some payload)"))))
        }
      case None => println("testLogs are empty at position " + 4)
    }

    //C receives from A, D receives from A,  B receives from C
    logs.lift(5) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(MessageLit("A", "B", 1, "Broadcast(Some payload)"),
          MessageLit("B", "A", 2, "Broadcast(Some payload)")), List("Start"))
        test("Testing AkkaParser, different senders sent multiple messages " +
          "with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1, "Broadcast(Some payload)"),
            Row("A", "D", 1, "Broadcast(Some payload)"), Row("C", "B", 3, "Broadcast(Some payload)"))))
        }
      case None => println("testLogs are empty at position " + 5)
    }

    //C receives from A, D receives from A,  B receives from A
    logs.lift(6) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val injections =
          List(
            MessageLit("A", "B", 1, "Broadcast(Some payload)"),
            MessageLit("B", "A", 2, "Broadcast(Some payload)"))
        val res: FormattedLogs = AkkaParser.parse(src, injections.toSet, List("Start"))
        test(
          "Testing AkkaParser, same sender sent multiple messages " +
          "with injection at first step & \"ghost\" time at step 2") {
          val rows =
            List(
              Row("A", "C", 1, "Broadcast(Some payload)"),
              Row("A", "D", 1, "Broadcast(Some payload)"),
              Row("A", "B", 3, "Broadcast(Some payload)"))
          assert(res == FormattedLogs(rows))
        }
      case None => println("testLogs are empty at position " + 6)
    }

    //B receives from A, B receives from A,  A receives from B, B receives from A
    logs.lift(7) match {
      case Some(input) =>
        val src = Source.fromString(input)
        val injections = List(MessageLit("B", "A", 3, "Broadcast(Some payload)"))
        val res: FormattedLogs = AkkaParser.parse(src, injections.toSet, List("Start"))
        test("Testing AkkaParser, same sender sent multiple messages with injection at third step") {
          val rows =
            List(
              Row("A", "B", 1, "Broadcast(Some payload)"),
              Row("A", "B", 2, "Broadcast(Some payload)"),
              Row("A", "B", 4, "Broadcast(Some payload)"))
          assert(res == FormattedLogs(rows))
        }
      case None => println("testLogs are empty at position " + 7)
    }

  }

  def testmanageClock(): Unit = {

    val inj1 = List(MessageLit("A", "C", 1, "Broadcast(Some payload)"))
    test("Assert that time does not tick when same injection sender and not injected") {
      assert(AkkaParser.manageClock("A", "B", "", "", 0, "Broadcast(Some payload)", inj1) == 1)
    }

    val inj2 = List(MessageLit("B", "A", 1, "Broadcast(Some payload)"))
    test("Assert that time ticks when not same injection sender") {
      assert(AkkaParser.manageClock("A", "B", "", "", 0, "Broadcast(Some payload)", inj2) == 2)
    }

    val inj3 = List(MessageLit("A", "B", 1, "Broadcast(Some payload)"))
    test("Assert that time ticks when same injection sender and injected") {
      assert(AkkaParser.manageClock("A", "B", "", "", 0, "Broadcast(Some payload)", inj3) == 2)
    }

    val inj4 = List(MessageLit("A", "B", 1, "Broadcast(Some payload)"), MessageLit("B", "A", 2, "Broadcast(Some payload)"))
    test("Assert that time ticks two steps when same injection and next senderinjector is different"){
      assert(AkkaParser.manageClock("A", "B", "", "", 0, "Broadcast(Some payload)", inj4) == 3)
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


  test("Testing AkkaParser.parseMessage"){

    val line: String = "DEBUG[system-akka.actor.default-dispatcher-3]akka://system/user/C-receivedhandledmessageBroadcast(Somepayload)fromActor[akka://system/user/A#-1746850710]"
    val message = AkkaParser.parseMessage(line)

    message shouldBe ("Broadcast(Somepayload)")

  }

}
