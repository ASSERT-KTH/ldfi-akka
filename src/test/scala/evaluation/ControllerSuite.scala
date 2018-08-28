package evaluation

import ldfi.akka.booleanformulas._
import ldfi.akka.evaluation.Controller._
import org.scalatest.{FunSuite, Matchers}

class ControllerSuite extends FunSuite with Matchers {

  val msg: MessageLit = MessageLit("A", "B", 1)("")
  val node1: Node = Node("A", 1)
  val node2: Node = Node("B", 1)
  val curSen: String = "A"
  val curRec: String = "B"

  test("testing Controller.isInjected, is not injected") {
    val inj: Set[Literal] = Set.empty
    val (injected, _) = isInjected(curSen, curRec, inj, 1, "")
    !injected
  }

  test("testing Controller.isInjected, is injected") {
    val inj: Set[Literal] = Set(msg.asInstanceOf[Literal])
    val (injected, _) = isInjected(curSen, curRec, inj, 1, "")
    injected
  }

  test("testing Controller.isInjected, sender crashed") {
    val inj: Set[Literal] = Set(node1.asInstanceOf[Literal])
    val (injected, _) = isInjected(curSen, curRec, inj, 1, "")
    injected
  }

  test("testing Controller.isInjected, recipient crashed") {
    val inj: Set[Literal] = Set(node2.asInstanceOf[Literal])
    val (injected, _) = isInjected(curSen, curRec, inj, 1, "")
    injected
  }

  test("testing Controller.isInjected, both nodes crashed") {
    val inj: Set[Literal] =
      Set(node1.asInstanceOf[Literal], node2.asInstanceOf[Literal])
    val (injected, _) = isInjected(curSen, curRec, inj, 1, "")
    injected
  }

  test("testing Controller.isInjected, msg cut & both crashed") {
    val inj: Set[Literal] = Set(msg.asInstanceOf[Literal],
                                node1.asInstanceOf[Literal],
                                node2.asInstanceOf[Literal])
    val (injected, _) = isInjected(curSen, curRec, inj, 1, "")
    injected
  }

  //Potentially needs some more edge-cases.
  test("testing Controller.manageClock") {
    val formula = new Formula
    val c1 = new Clause(formula)
    val c2 = new Clause(formula)

    val m1 = MessageLit("A", "B", 1)("")
    val m2 = MessageLit("B", "C", 2)("")
    val m3 = MessageLit("C", "D", 3)("")
    val m4 = MessageLit("C", "D", 2)("")

    val m1tom3 = List(m1, m2, m3)
    m1tom3.foreach(c1.addLiteralToClause)

    val m1andmr = List(m1, m4)
    m1andmr.foreach(c2.addLiteralToClause)

    //c1 gets id 1 and c2 and 2
    formula.addClause(c1)
    formula.addClause(c2)

    val clauses = List(c1, c2)

    manageClock("A", "B", "", clauses, Map(1 -> 0, 2 -> 0)) shouldEqual Map(
      1 -> 1,
      2 -> 1)

    manageClock("C", "D", "", clauses, Map(1 -> 1, 2 -> 1)) shouldEqual Map(
      1 -> 3,
      2 -> 2)

  }

}
