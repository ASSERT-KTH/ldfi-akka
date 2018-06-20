package evaluation

import java.util.concurrent
import java.util.concurrent.ConcurrentLinkedQueue
import collection.JavaConverters._

import akka.actor.ActorRef
import akka.dispatch.Envelope
import ldfi.akka.booleanformulas._
import ldfi.akka.evaluation.Controller._
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable


class ControllerSuite extends FunSuite with Matchers {

  val msg = Message("A", "B", 1)
  val node1 = Node("A", 1)
  val node2 = Node("B", 1)
  val curSen = "A"
  val curRec = "B"

  test("Testing Controller.checkInjection, is not injected"){
    val inj : List[Literal] = List.empty
    !isInjected(curSen, curRec, inj, 1)
  }

  test("Testing Controller.checkInjection, is injected"){
    val inj : List[Literal] = List(msg)
    isInjected(curSen, curRec, inj, 1)
  }

  test("Testing Controller.checkInjection, sender crashed"){
    val inj : List[Literal] = List(node1)
    isInjected(curSen, curRec, inj, 1)
  }

  test("Testing Controller.checkInjection, recipient crashed"){
    val inj : List[Literal] = List(node2)
    isInjected(curSen, curRec, inj, 1)
  }

  test("Testing Controller.checkInjection, both nodes crashed"){
    val inj : List[Literal] = List(node1, node2)
    isInjected(curSen, curRec, inj, 1)
  }

  test("Testing Controller.checkInjection, msg cut & both crashed"){
    val inj : List[Literal] = List(msg, node1, node2)
    isInjected(curSen, curRec, inj, 1)
  }

  test("Testing Controller.getCurrentTime"){
    val curSen = "A"
    val curRec = "B"
    manageClock(curSen, curRec) should equal (1)
  }

  test("Testing Controller.dispatchQueue"){

    val A : ActorRef = null
    val B : ActorRef = null
    val C : ActorRef = null

    val msg1 = Message("A", "B", 1)
    val msg2 = Message("B", "A", 2)
    val msg3 = Message("C", "A", 3)

    val env1 = Envelope("hello", A)
    val env2 = Envelope("howdy", B)
    val env3 = Envelope("greetings", C)

    val forcedSched = mutable.Queue(SuperEnvelope(msg2, env2), SuperEnvelope(msg1, env1))
    val newMessages = mutable.Queue(SuperEnvelope(msg3, env3))
    val queue = new ConcurrentLinkedQueue[Envelope]()
    val initialScheduleMap = Map(msg1 -> 1, msg2 -> 2)

    dispatchQueue(queue, initialScheduleMap, forcedSched, newMessages)

    var resultingQueue = new ConcurrentLinkedQueue[Envelope]()
    resultingQueue.add(env1)
    resultingQueue.add(env2)
    resultingQueue.add(env3)

    //convert java collections to scala collections, since no structural equality in Java
    queue.asScala.toSet should be (resultingQueue.asScala.toSet)


  }



}
