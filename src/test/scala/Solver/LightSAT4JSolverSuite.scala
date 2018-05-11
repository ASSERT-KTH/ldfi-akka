package Solver

import ldfi.akka.BooleanFormulas.BooleanFormula._
import ldfi.akka.BooleanFormulas.LightSAT4JSolver
import ldfi.akka.FailureSpec
import org.sat4j.core.VecInt
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer

class LightSAT4JSolverSuite extends FunSuite {

  testLightSat4JSolver()
  testremoveSuperSets()
  testconvertLitsToVecInt()
  testconvertLitsToNegatedVecInt()


  def testLightSat4JSolver(): Unit = {
    val formula = new Formula
    val clause = new Clause

    val msg1 = Message("A", "B", 1)
    val msg2 = Message("A", "C", 1)

    val node1 = Node("A", 1)
    val node2 = Node("B", 1)
    val node3 = Node("C", 1)

    val msgs = Set(msg1, msg2)
    val nodes = Set(node1, node2, node3)

    msgs.foreach(msg => clause.addLiteralToClause(msg))
    nodes.foreach(n => clause.addLiteralToClause(n))
    formula.addClause(clause)

    //fpsec1
    val failureSpec1 = FailureSpec(3, 2, 0, nodes, msgs, Set.empty, Set.empty)
    val expected1 = Set(Set(msg1), Set(msg2))
    test("Testing LightSAT4jSolver with fspec: <3, 2, 0>") {
      assert(LightSAT4JSolver.solve(formula, failureSpec1) == expected1)
    }

    //fpsec2
    val failureSpec2 = FailureSpec(3, 2, 1, nodes, msgs, Set.empty, Set.empty)
    val expected2 = Set(Set(msg1), Set(msg2), Set(node1), Set(node2), Set(node3))
    test("Testing LightSAT4jSolver with fspec: <3, 2, 1>") {
      assert(LightSAT4JSolver.solve(formula, failureSpec2) == expected2)
    }

    //fpsec3
    val failureSpec3 = FailureSpec(3, 1, 0, nodes, msgs, Set.empty, Set.empty)
    val expected3 = Set.empty
    test("Testing LightSAT4jSolver with fspec: <3, 1, 0>") {
      assert(LightSAT4JSolver.solve(formula, failureSpec3) == expected3)
    }

    //fpsec4
    val failureSpec4 = FailureSpec(3, 1, 1, nodes, msgs, Set.empty, Set.empty)
    val expected4 = Set(Set(node1), Set(node2), Set(node3))
    test("Testing LightSAT4jSolver with fspec: <3, 1, 1>") {
      assert(LightSAT4JSolver.solve(formula, failureSpec4) == expected4)
    }

    //fpsec5
    val failureSpec5 = FailureSpec(3, 0, 0, nodes, msgs, Set.empty, Set.empty)
    val expected5 = Set.empty
    test("Testing LightSAT4jSolver with fspec: <3, 0, 0>") {
      assert(LightSAT4JSolver.solve(formula, failureSpec5) == expected5)
    }

    //fpsec6
    val failureSpec6 = FailureSpec(3, 0, 1, nodes, msgs, Set.empty, Set.empty)
    val expected6 = Set(Set(node1), Set(node2), Set(node3))
    test("Testing LightSAT4jSolver with fspec: <3, 0, 1>") {
      assert(LightSAT4JSolver.solve(formula, failureSpec6) == expected6)
    }

    //fpsec7
    val failureSpec7 = FailureSpec(3, 2, 1, nodes, msgs, Set(Node("A", 1)), Set.empty)
    val expected7 = Set(Set(msg1), Set(msg2), Set(node2), Set(node3))
    test("Testing LightSAT4jSolver with fspec: <3, 2, 1> with one node crashed already") {
      assert(LightSAT4JSolver.solve(formula, failureSpec7) == expected7)
    }

    //fpsec8
    val failureSpec8 = FailureSpec(3, 2, 1, nodes, msgs, Set(Node("A", 1)), Set.empty)
    val expected8 = Set(Set(msg1), Set(msg2), Set(node2), Set(node3))
    test("Testing LightSAT4jSolver with fspec: <3, 2, 1> with one node crashed already") {
      assert(LightSAT4JSolver.solve(formula, failureSpec8) == expected8)
    }

    //fpsec9
    val failureSpec9 = FailureSpec(3, 2, 1, nodes, msgs, Set(node1), Set(msg1))
    val expected9 = Set(Set(msg2), Set(node2), Set(node3))
    test("Testing LightSAT4jSolver with fspec: <3, 2, 1> with one node crashed and one message cut") {
      assert(LightSAT4JSolver.solve(formula, failureSpec9) == expected9)
    }

    //fpsec10
    val failureSpec10 = FailureSpec(3, 2, 3, nodes, msgs, Set(node1, node2, node3), Set(msg1, msg2))
    val expected10 = Set.empty
    test("Testing LightSAT4jSolver with fspec: <3, 2, 3> with all nodes crashed and all messages cut") {
      assert(LightSAT4JSolver.solve(formula, failureSpec10) == expected10)
    }

  }

  def testremoveSuperSets(): Unit = {
    val models = ListBuffer(
      Set(Message("A", "B", 1).asInstanceOf[Literal]),
      Set(Message("A", "B", 1).asInstanceOf[Literal], Message("A", "C", 1).asInstanceOf[Literal]),
      Set(Node("A", 1).asInstanceOf[Literal]),
      Set(Node("A", 1).asInstanceOf[Literal], Node("B", 1).asInstanceOf[Literal]))

    test("Testing removeSuperSets"){
      assert(LightSAT4JSolver.removeSuperSets(models, models) ==
        Set(Set(Message("A", "B", 1)), Set(Node("A", 1))))
    }
  }

  def testconvertLitsToVecInt(): Unit = {
    //Helper method that adds 2 messages and 3 nodes to a clause which is in turn is added to a formula
    val formula = generateCNFFormula()
    val literals = formula.getAllLiterals

    //Has to be converted to Set since literals are unordered in the map. The important part is that
    //all nodes exist in the map and they have ids from 1 to 5.
    val expected = Set(1, 2, 3, 4, 5)
    val res = LightSAT4JSolver.convertLitsToVecInt(literals).toArray.toSet

    test("Testing convertLitsToVecInt"){
      assert(res == expected)
    }

  }

  def testconvertLitsToNegatedVecInt(): Unit = {
    //Helper method that adds 2 messages and 3 nodes to a clause which is in turn is added to a formula
    val formula = generateCNFFormula()
    val literals = formula.getAllLiterals

    //Has to be converted to Set since literals are unordered in the map. The important part is that
    //all nodes exist in the map and they have ids from 1 to 5.
    val expected = Set(-1, -2, -3, -4, -5)
    val res = LightSAT4JSolver.convertLitsToNegatedVecInt(literals).toArray.toSet

    test("Testing convertLitsToNegatedVecInt"){
      assert(res == expected)
    }

  }


  //Helper method for convertLitsTo(Negated)VecInt
  def generateCNFFormula(): Formula = {
    val formula = new Formula
    val clause = new Clause
    val msgs = Set(Message("A", "B", 1),  Message("A", "C", 1))
    val nodes = Set(Node("A", 1), Node("B", 1), Node("C", 1))
    msgs.foreach(msg => clause.addLiteralToClause(msg))
    nodes.foreach(n => clause.addLiteralToClause(n))
    formula.addClause(clause)
    formula
  }








}