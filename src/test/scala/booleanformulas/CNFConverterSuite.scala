package booleanformulas

import ldfi.akka.booleanformulas._
import ldfi.akka.parser.AkkaParser.Row
import org.scalatest.{FunSuite, Matchers}

class CNFConverterSuite extends FunSuite with Matchers {

  val formula: Formula = new Formula
  val clause: Clause = new Clause(formula)
  val row: Row = Row("A", "B", 1, "")
  val msg: MessageLit = MessageLit("A", "B", 1)("")

  test("Testing CNFConverter.addRowToClause") {
    CNFConverter.addRowToClause(clause, row)
    clause.getLiteralsInClause should contain(msg)
  }

  test("Testing CNFConverter.getMessage") {
    CNFConverter.getMessage(row) shouldEqual MessageLit("A", "B", 1)("")
  }

  test("Testing CNFConverter.get(sender/recipient)Node") {
    CNFConverter.getSenderNode(row) shouldEqual Node("A", 1)
    CNFConverter.getRecipientNode(row) shouldEqual Node("B", 1)
  }

}
