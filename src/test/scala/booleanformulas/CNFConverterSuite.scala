package booleanformulas

import ldfi.akka.booleanformulas.BooleanFormula._
import ldfi.akka.booleanformulas._
import ldfi.akka.parser.AkkaParser.Row
import org.scalatest.FunSuite

class CNFConverterSuite extends FunSuite {



  testaddLiteralsToClause()
  testgetMessage()
  testgetNode()

  def testaddLiteralsToClause(): Unit = {
    val formula = new Formula
    val clause = new Clause(formula)
    val row = Row("A", "B", 1)
    val msg = Message("A", "B", 1)

    CNFConverter.addRowToClause(clause, row)
    test("testing addLiteralToClause"){
      assert(formula.literalExistsInFormula(msg))
    }
  }

  def testgetMessage(): Unit = {
    val row = Row("A", "B", 1)
    val msg = CNFConverter.getMessage(row)
    test("testing getMessage") {
      assert(msg.sender == row.sender)
      assert(msg.recipient == row.recipient)
      assert(msg.time == row.time)
    }
  }

  def testgetNode(): Unit = {
    val row = Row("A", "B", 1)
    val msg = CNFConverter.getSenderNode(row)
    test("testing getNode") {
      assert(msg.node == row.sender)
      assert(msg.time == row.time)
    }
  }

}
