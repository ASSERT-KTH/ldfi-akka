package booleanformulas

import ldfi.akka.booleanformulas._
import ldfi.akka.parser.AkkaParser.Row
import org.scalatest.{FunSuite, Matchers}

class CNFConverterSuite extends FunSuite with Matchers {

  test("Testing CNFConverter.addRowToClause"){
    val formula = new Formula
    val clause = new Clause(formula)
    val row = Row("A", "B", 1)
    val msg = Message("A", "B", 1)

    CNFConverter.addRowToClause(clause, row)

    clause.literalExistsInClause(msg)
  }

  test("Testing CNFConverter.getMessage"){
    val row = Row("A", "B", 1)
    val msg = CNFConverter.getMessage(row)

    msg.sender should be (row.sender)
    msg.recipient should be (row.recipient)
    msg.time should be (row.time)
  }

  test("Testing CNFConverter.getNode"){
    val row = Row("A", "B", 1)
    val msg = CNFConverter.getSenderNode(row)

    msg.node should be (row.sender)
    msg.time should be (row.time)
  }

}
