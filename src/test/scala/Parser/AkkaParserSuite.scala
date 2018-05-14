package Parser

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

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
    val logs = Source.fromFile("src/test/scala/Parser/testLogs.log").mkString.split("\n\n")

    //B receives from A, C receives from A
    logs.lift(0) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set.empty)
        test("Testing AkkaParser, two messages sent with no injections") {
          assert(res == FormattedLogs(List(Row("A", "B", 1), Row("A", "C", 1))))
        }
      case None => println("testLogs are empty at position " + 0)
    }

    //B receives from A
    logs.lift(1) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(Message("A", "C", 1)))
        test("Testing AkkaParser, two messages sent with single injection at time 1") {
          assert(res == FormattedLogs(List(Row("A", "B", 1))))
        }
      case None => println("testLogs are empty at position " + 1)
    }

    //B receives from A
    logs.lift(1) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(Message("A", "B", 1), Message("B", "A", 2)))
        test("Testing AkkaParser, one messages sent with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "B", 3))))
        }
      case None => println("testLogs are empty at position " + 1)
    }

    //C receives from A, C receives from A
    logs.lift(2) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(Message("A", "B", 1), Message("B", "A", 2)))
        test("Testing AkkaParser, two same messages sent with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1), Row("A", "C", 3))))
        }
      case None => println("testLogs are empty at position " + 2)
    }

    //C receives from A, B receives from A
    logs.lift(3) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(Message("A", "B", 1), Message("B", "A", 2)))
        test("Testing AkkaParser, two diff messages sent with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1), Row("A", "B", 3))))
        }
      case None => println("testLogs are empty at position " + 3)
    }

    //C receives from A, B receives from C
    logs.lift(4) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(Message("A", "B", 1), Message("B", "A", 2)))
        test("Testing AkkaParser, different senders sent with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1), Row("C", "B", 3))))
        }
      case None => println("testLogs are empty at position " + 4)
    }

    //C receives from A, D receives from A,  B receives from C
    logs.lift(5) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(Message("A", "B", 1), Message("B", "A", 2)))
        test("Testing AkkaParser, different senders sent multiple messages " +
          "with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1), Row("A", "D", 1), Row("C", "B", 3))))
        }
      case None => println("testLogs are empty at position " + 5)
    }

    //C receives from A, D receives from A,  B receives from A
    logs.lift(6) match {
      case Some(input) =>
        println(input)
        val src = Source.fromString(input)
        val res: FormattedLogs = AkkaParser.parse(src, Set(Message("A", "B", 1), Message("B", "A", 2)))
        test("Testing AkkaParser, same sender sent multiple messages " +
          "with injection at first step & \"ghost\" time at step 2") {
          assert(res == FormattedLogs(List(Row("A", "C", 1), Row("A", "D", 1), Row("A", "B", 3))))
        }
      case None => println("testLogs are empty at position " + 6)
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

    val inj4 = Set(Message("A", "B", 1).asInstanceOf[Literal], Message("B", "A", 2).asInstanceOf[Literal])
    test("Assert that time ticks two steps when same injection and next senderinjector is different"){
      assert(AkkaParser.manageClock("A", "B", "", 0, inj4) == 3)
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
