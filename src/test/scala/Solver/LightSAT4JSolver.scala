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
    val failureSpec1 = FailureSpec(2, 2, 0, nodes, msgs, Set.empty, Set.empty)
    val expected1 = Set(Set(msg1), Set(msg2))
    test("Testing LightSAT4jSolver with fspec: <2, 2, 0>") {
      assert(LightSAT4JSolver.solve(formula, failureSpec1).toSet == expected1)
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







}