package evaluation

import ldfi.akka.booleanformulas._
import ldfi.akka.evaluation.Controller._
import org.scalatest.FunSuite

class ControllerSuite extends FunSuite {

  testcheckInjections()
  testgetCurrentTime()

  def testcheckInjections(): Unit = {

    val msg = MessageLit("A", "B", 1, "")
    val node1 = Node("A", 1)
    val node2 = Node("B", 1)
    val curSen = "A"
    val curRec = "B"

    val inj1: Set[Literal] = Set.empty
    test("testing checkInjection, is not injected") {
      assert(!isInjected(curSen, curRec, inj1, 1, ""))
    }

    val inj2: Set[Literal] = Set(msg.asInstanceOf[Literal])
    test("testing checkInjection, is injected") {
      assert(isInjected(curSen, curRec, inj2, 1, ""))
    }

    val inj3: Set[Literal] = Set(node1.asInstanceOf[Literal])
    test("testing checkInjection, sender crashed") {
      assert(isInjected(curSen, curRec, inj3, 1, ""))
    }

    val inj4: Set[Literal] = Set(node2.asInstanceOf[Literal])
    test("testing checkInjection, recipient crashed") {
      assert(isInjected(curSen, curRec, inj4, 1, ""))
    }

    val inj5: Set[Literal] =
      Set(node1.asInstanceOf[Literal], node2.asInstanceOf[Literal])
    test("testing checkInjection, both nodes crashed") {
      assert(isInjected(curSen, curRec, inj5, 1, ""))
    }

    val inj6: Set[Literal] = Set(msg.asInstanceOf[Literal],
                                 node1.asInstanceOf[Literal],
                                 node2.asInstanceOf[Literal])
    test("testing checkInjection, msg cut & both crashed") {
      assert(isInjected(curSen, curRec, inj6, 1, ""))
    }
  }

  def testgetCurrentTime(): Unit = {
    val curSen = "A"
    val curRec = "B"

    test("testing getCurrentTime") {
      assert(manageClock(curSen, curRec) == 1)
    }
  }

}
