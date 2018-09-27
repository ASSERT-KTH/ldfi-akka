package parser

import ldfi.akka.booleanformulas._
import ldfi.akka.parser.AkkaParser
import ldfi.akka.parser.AkkaParser.{FormattedLogs, Row}
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class AkkaParserSuite extends FunSuite with Matchers {

  val logs: Array[String] = Source
    .fromFile("src/test/scala/parser/testLogs.log")
    .mkString
    .split("\n\n")

  //B receives from A, C receives from A
  logs.lift(0) match {
    case Some(input) =>
      val src = Source.fromString(input)
      val res = AkkaParser.parse(src, List("Start"))
      test("Testing AkkaParser.parse: B received from A, C received from A") {
        assert(
          res == FormattedLogs(
            List(Row("A", "B", 1, "Broadcast(Some payload)"),
              Row("A", "C", 2, "Broadcast(Some payload)"))))
      }
    case None => println("testLogs are empty at position " + 0)
  }

  //B receives from A
  logs.lift(1) match {
    case Some(input) =>
      val src = Source.fromString(input)
      val res = AkkaParser.parse(src, List("Start"))
      test("Testing AkkaParser.parse B received from A") {
        assert(
          res == FormattedLogs(
            List(Row("A", "B", 1, "Broadcast(Some payload)"))))
      }
    case None => println("testLogs are empty at position " + 1)
  }

  //C receives from A, C receives from A
  logs.lift(2) match {
    case Some(input) =>
      val src = Source.fromString(input)
      val res = AkkaParser.parse(src, List("Start"))
      test("Testing AkkaParser.parse: C received from A, C received from A ") {
        val rows = List(Row("A", "C", 1, "Broadcast(Some payload)"),
          Row("A", "C", 2, "Broadcast(Some payload)"))
        assert(res == FormattedLogs(rows))
      }
    case None => println("testLogs are empty at position " + 2)
  }

  //C receives from A, B receives from A
  logs.lift(3) match {
    case Some(input) =>
      val src = Source.fromString(input)
      val res = AkkaParser.parse(src, List("Start"))
      test("Testing AkkaParser.parse: C received from A, B received from A") {
        assert(
          res == FormattedLogs(
            List(Row("A", "C", 1, "Broadcast(Some payload)"),
              Row("A", "B", 2, "Broadcast(Some payload)"))))
      }
    case None => println("testLogs are empty at position " + 3)
  }

  //C receives from A, B receives from C
  logs.lift(4) match {
    case Some(input) =>
      val src = Source.fromString(input)
      val res = AkkaParser.parse(src, List("Start"))
      test("Testing AkkaParser.parse: C received from A, B received from C") {
        assert(
          res == FormattedLogs(
            List(Row("A", "C", 1, "Broadcast(Some payload)"),
              Row("C", "B", 2, "Broadcast(Some payload)"))))
      }
    case None => println("testLogs are empty at position " + 4)
  }

  //C receives from A, D receives from A,  B receives from C
  logs.lift(5) match {
    case Some(input) =>
      val src = Source.fromString(input)
      val res = AkkaParser.parse(src, List("Start"))
      test(
        "Testing AkkaParser.parse: C received from A, D received from A, B received from C") {
        assert(
          res == FormattedLogs(
            List(Row("A", "C", 1, "Broadcast(Some payload)"),
              Row("A", "D", 2, "Broadcast(Some payload)"),
              Row("C", "B", 3, "Broadcast(Some payload)"))))
      }
    case None => println("testLogs are empty at position " + 5)
  }

  test("Testing AkkaParser.parseSender") {
    val line =
      "DEBUG[system-akka.actor.default-dispatcher-3]akka://system/user/C-receivedhandledmessageBroadcast(Somepayload)fromActor[akka://system/user/A#-1746850710]"
    val sender = AkkaParser.parseSender(line)
    sender shouldEqual "A"
  }

  test("Testing AkkaParser.parseRecipient") {
    val localLine =
      "DEBUG[system-akka.actor.default-dispatcher-3]akka://system/user/C-receivedhandledmessageBroadcast(Somepayload)fromActor[akka://system/user/A#-1746850710]"
    val clusterLine =
      "DEBUG[ExampleSpec-akka.actor.default-dispatcher-18]akka.tcp://ExampleSpec@localhost:34973/user/batching-server-receivedhandledmessageEvent(ClusterListenerIs(Actor[akka://ExampleSpec/user/raft-cluster#-26111901]),Data(Log(0,List()),Term(0),Set(),None,0))fromActor[akka://ExampleSpec/user/raft-cluster#-26111901]"

    val localRec = AkkaParser.parseRecipient(localLine)
    val clusterRec = AkkaParser.parseRecipient(clusterLine)

    localRec shouldEqual "C"
    clusterRec shouldEqual "batching-server"
  }

  test("Testing AkkaParser.parseMessage") {
    val line =
      "DEBUG[system-akka.actor.default-dispatcher-3]akka://system/user/C-receivedhandledmessageBroadcast(Somepayload)fromActor[akka://system/user/A#-1746850710]"
    val message = AkkaParser.parseMessage(line)
    message shouldEqual "Broadcast(Somepayload)"
  }

}
