package booleanformulas

import org.scalatest.{FunSuite, Matchers}
import ldfi.akka.booleanformulas.BooleanFormula._

class BooleanFormulasSuite extends FunSuite with Matchers {


  test("Testing addclause"){
      val formula = new Formula
      val clause = new Clause(formula)
      formula.addClause(clause)
      formula.clauses should contain (clause)
    }

  test("Testing formula.getAllLiterals"){
    val msg1 = Message("A", "B", 1)
    val node1 = Node("A", 1)
    val node2 = Node("B", 1)

    val msg2 = Message("C", "D", 1)
    val node3 = Node("C", 1)
    val node4 = Node("D", 1)

    val (formula, _, _, _) = generateFormula()

    formula.getAllLiterals should be (Set(msg1, node1, node2, msg2, node3, node4))
  }

  test("Testing formula.getAllNodes"){
    val (formula, _, nodes, _) = generateFormula()

    formula.getAllNodes should be (nodes.toSet)
  }

  test("Testing formula.getAllMessages"){
    val (formula, _, _, messages) = generateFormula()

    formula.getAllMessages.toSet should be (messages.toSet)
  }

  test("Testing formula.literalExistsInFormula"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)

    formula.addClause(clause)
    clause.addLiteralToClause(msg)

    formula.literalExistsInFormula(msg)
  }

  test("Testing formula.idExistsInLieteralsToId"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)

    formula.addClause(clause)
    //Should have id 1
    clause.addLiteralToClause(msg)

    formula.idExistsInLiteralsToId(1)
  }

  test("Testing formula.litExistsInIdToLiterals"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)
    formula.addClause(clause)
    clause.addLiteralToClause(msg)

    formula.litExistsInIdToLiterals(msg)
  }

  test("Testing formula.getLitIdCnt") {
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)
    formula.addClause(clause)
    clause.addLiteralToClause(msg)

    //since only one lit, next litidcnt should be 2
    formula.getLitIdCnt should be (2)
  }

  test("Testing formula.getLatestTime") {
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)
    formula.addClause(clause)
    clause.addLiteralToClause(msg)

    //since only one message, latest time should be 1
    formula.getLatestTime should be (1)
  }



  test("Testing formula.getActivityTimeRange"){
    val formula = new Formula
    val clause = new Clause(formula)

    val act1 = Node("A", 1)
    val act2 = Node("A", 2)

    formula.addClause(clause)
    clause.addLiteralToClause(act1)
    clause.addLiteralToClause(act2)

    formula.getActivityTimeRange("A") should be ((1, 2))
  }

  test("Testing formula.getLiteralId"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)

    clause.addLiteralToClause(msg)
    formula.addClause(clause)
    formula.getLiteralId(msg) should be (1)
  }

  test("Testing formula.updateActivityMap") {
    val formula = new Formula
    val msg = Message("A", "B", 1)

    formula.updateActivityMap(msg)

    formula.activityTimeRange should contain allOf ("A" -> (1, 1), "B" -> (1, 1))
  }

  test("Testing formula.updateSenderTime"){
    val formula = new Formula
    val msg1 = Message("A", "B", 1)
    val msg2 = Message("A", "B", 2)

    formula.updateSenderTime(msg1)
    formula.firstMessageSent.get("A") should be (Some(1))

    formula.updateSenderTime(msg2)
    formula.firstMessageSent.get("A") should be (Some(2))
  }

  test("Testing clause.addLiteralToClause"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)

    clause.addLiteralToClause(msg)

    clause.literals should contain (msg)
  }

  test("Testing clause.getMessageInClause"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)
    val act = Node("A", 1)

    clause.addLiteralToClause(msg)
    clause.addLiteralToClause(act)

    clause.getMessagesInClause should contain only msg
  }

  test("Testing clause.getLiteralInClause"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)
    val node = Node("A", 1)

    clause.addLiteralToClause(msg)
    clause.addLiteralToClause(node)

    clause.getNodesInClause should contain only node
  }

  test("Testing clause.literalExistsInClause"){
    val formula = new Formula
    val clause = new Clause(formula)
    val msg = Message("A", "B", 1)
    val node = Node("A", 1)

    clause.addLiteralToClause(msg)
    clause.addLiteralToClause(node)

    clause.literalExistsInClause(msg)
  }

  //helper function
  def generateFormula(): (Formula, List[Clause], List[Node], List[Message]) = {

    val msg1 = Message("A", "B", 1)
    val node1 = Node("A", 1)
    val node2 = Node("B", 1)


    val msg2 = Message("C", "D", 1)
    val node3 = Node("C", 1)
    val node4 = Node("D", 1)

    val formula = new Formula
    val c1 = new Clause(formula)
    val c2 = new Clause(formula)

    formula.addClause(c1)
    formula.addClause(c2)

    c1.addLiteralToClause(msg1)
    c1.addLiteralToClause(node1)
    c1.addLiteralToClause(node2)

    c2.addLiteralToClause(msg2)
    c2.addLiteralToClause(node3)
    c2.addLiteralToClause(node4)

    (formula, List(c1, c2), List(node1, node2, node3, node4), List(msg1, msg2))

  }

}
