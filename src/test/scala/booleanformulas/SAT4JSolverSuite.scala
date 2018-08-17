package booleanformulas

import ldfi.akka.FailureSpec
import ldfi.akka.booleanformulas._
import ldfi.akka.booleanformulas.SAT4JSolver
import org.scalatest.{FunSuite, Matchers}

class SAT4JSolverSuite extends FunSuite with Matchers {

  val formula = new Formula
  val clause = new Clause(formula)

  val msg1 = MessageLit("A", "B", 1, "")
  val msg2 = MessageLit("B", "C", 2, "")

  val node1 = Node("A", 1)
  val node2 = Node("B", 1)
  val node3 = Node("C", 2)
  val node4 = Node("B", 2)

  val msgs = Set(msg1, msg2)
  val nodes = Set(node1, node2, node3, node4)

  msgs.foreach(msg => clause.addLiteralToClause(msg))
  nodes.foreach(n => clause.addLiteralToClause(n))
  formula.addClause(clause)

  /** **************************************************************
    * FORMULA:
    * M(A, B, 1) ∨ M(B, C, 2) ∨ P(A, 1) ∨ P(B, 1) ∨ P(B, 2) v P(C, 2)
    * ************************************************************/
  //fpsec1
  test("Testing SAT4JSolver with fspec: <3, 2, 0>") {
    val failureSpec = FailureSpec(3, 2, 0, nodes, msgs, Set.empty, Set.empty)
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set(Set(msg1),
                                                            Set(msg2))
  }

  //fpsec2
  test("Testing SAT4JSolver with fspec: <3, 2, 1>") {
    val failureSpec = FailureSpec(3, 2, 1, nodes, msgs, Set.empty, Set.empty)
    val expected2 =
      Set(Set(msg1), Set(msg2), Set(node1), Set(node2), Set(node3))
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set(Set(node1),
                                                            Set(node2),
                                                            Set(node3),
                                                            Set(node4),
                                                            Set(msg1),
                                                            Set(msg2))
  }

  //fpsec3
  test("Testing SAT4JSolver with fspec: <3, 1, 0>") {
    val failureSpec = FailureSpec(3, 1, 0, nodes, msgs, Set.empty, Set.empty)
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set(Set(msg2))
  }

  //fpsec4
  test("Testing SAT4JSolver with fspec: <3, 1, 1>") {
    val failureSpec = FailureSpec(3, 1, 1, nodes, msgs, Set.empty, Set.empty)
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set(Set(msg2),
                                                            Set(node1),
                                                            Set(node2),
                                                            Set(node3),
                                                            Set(node4))
  }

  //fpsec5
  test("Testing SAT4JSolver with fspec: <3, 0, 0>") {
    val failureSpec = FailureSpec(3, 0, 0, nodes, msgs, Set.empty, Set.empty)
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set.empty

  }

  //fpsec6
  test("Testing SAT4JSolver with fspec: <3, 0, 1>") {
    val failureSpec = FailureSpec(3, 0, 1, nodes, msgs, Set.empty, Set.empty)
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set(Set(node1),
                                                            Set(node2),
                                                            Set(node3),
                                                            Set(node4))
  }

  //fpsec7
  test("Testing SAT4JSolver with fspec: <3, 2, 1> with one node crashed") {
    val failureSpec =
      FailureSpec(3, 2, 1, nodes, msgs, Set(node1), Set.empty)
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set(Set(msg1),
                                                            Set(msg2),
                                                            Set(node2),
                                                            Set(node3),
                                                            Set(node4))
  }

  //fpsec8
  test(
    "Testing SAT4JSolver with fspec: <3, 2, 1> with one node crash and msg cut") {
    val failureSpec = FailureSpec(3, 2, 1, nodes, msgs, Set(node1), Set(msg2))
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set(Set(msg1),
                                                            Set(node2),
                                                            Set(node3),
                                                            Set(node4))
  }

  //fpsec10
  test(
    "Testing SAT4JSolver with fspec: <3, 2, 3> with all nodes crashed and all messages cut") {
    val failureSpec = FailureSpec(3,
                                  2,
                                  4,
                                  nodes,
                                  msgs,
                                  Set(node1, node2, node3, node4),
                                  Set(msg1, msg2))
    SAT4JSolver.solve(formula, failureSpec) shouldEqual Set.empty
  }

  test("Testing removeSuperSets") {
    val models = List(
      Set(MessageLit("A", "B", 1, "").asInstanceOf[Literal]),
      Set(MessageLit("A", "B", 1, "").asInstanceOf[Literal],
          MessageLit("A", "C", 1, "").asInstanceOf[Literal]),
      Set(Node("A", 1).asInstanceOf[Literal]),
      Set(Node("A", 1).asInstanceOf[Literal],
          Node("B", 1).asInstanceOf[Literal])
    )
    assert(
      SAT4JSolver.removeSuperSets(models, models) ==
        Set(Set(MessageLit("A", "B", 1, "")), Set(Node("A", 1))))
  }

  test("Testing convertLitsToVecInt") {
    //Helper method that adds 2 messages and 3 nodes to a clause which is in turn is added to a formula
    val formula = generateCNFFormula()
    val literals = formula.getAllLiterals.toList

    //Has to be converted to Set since literals are unordered in the map. The important part is that
    //all nodes exist in the map and they have ids from 1 to 5.
    val expected = Set(1, 2, 3, 4, 5)
    val res = SAT4JSolver.convertLitsToVecInt(formula, literals).toArray.toSet
    assert(res == expected)
  }

  test("Testing convertLitsToNegatedVecInt") {
    //Helper method that adds 2 messages and 3 nodes to a clause which is in turn is added to a formula
    val formula = generateCNFFormula()
    val literals = formula.getAllLiterals.toList

    //Has to be converted to Set since literals are unordered in the map. The important part is that
    //all nodes exist in the map and they have ids from 1 to 5.
    val expected = Set(-1, -2, -3, -4, -5)
    val res =
      SAT4JSolver.convertLitsToNegatedVecInt(formula, literals).toArray.toSet
    assert(res == expected)
  }

  //Helper method for convertLitsTo(Negated)VecInt
  def generateCNFFormula(): Formula = {
    val formula = new Formula
    val clause = new Clause(formula)
    val msgs = Set(MessageLit("A", "B", 1, ""), MessageLit("A", "C", 1, ""))
    val nodes = Set(Node("A", 1), Node("B", 1), Node("C", 1))
    msgs.foreach(msg => clause.addLiteralToClause(msg))
    nodes.foreach(n => clause.addLiteralToClause(n))
    formula.addClause(clause)
    formula
  }

}
