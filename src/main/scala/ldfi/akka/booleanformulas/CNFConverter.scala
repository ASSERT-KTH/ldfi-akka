package ldfi.akka.booleanformulas

import ldfi.akka.parser.AkkaParser._

object CNFConverter {

  def run(formattedLog: FormattedLogs, formula: Formula): (Clause, Boolean) = {
    val clause = new Clause(formula)
    for (line <- formattedLog.rows) {
      addRowToClause(clause, line)
    }
    //prettyPrintClause(clause)
    val existsInFormula = formula.clauseExistInFormula(clause)
    formula.addClause(clause)
    (clause, existsInFormula)
  }

  def addRowToClause(clause: Clause, line: Row): Unit = {
    val messageLiteral = getMessage(line)
    val senderNodeLiteral = getSenderNode(line)
    val recipientNodeLiteral = getRecipientNode(line)

    if (!clause.literalExistsInClause(senderNodeLiteral))
      clause.addLiteralToClause(senderNodeLiteral)

    if (!clause.literalExistsInClause(recipientNodeLiteral))
      clause.addLiteralToClause(recipientNodeLiteral)

    //all messages are unique
    clause.addLiteralToClause(messageLiteral)
  }

  def getSenderNode(line: Row): Node = Node(line.sender, line.time)

  def getRecipientNode(line: Row): Node = Node(line.recipient, line.time)

  def getMessage(line: Row): MessageLit =
    MessageLit(line.sender, line.recipient, line.time)(line.message)

  //Helper functions
  def getPrettyClause(clause: Clause, nodes: Boolean): String = {
    clause.getLiteralsInClause
      .map {
        case Node(node, time) if nodes =>
          "P(" + node + ", " + time + ") ∨ "
        case Node(node, time) if !nodes =>
          ""
        case MessageLit(sender, recipient, time) =>
          "M(" + sender + ", " + recipient + ", " + time + ") ∨ "
      }
      .reduceLeft(_ ++ _)
      .dropRight(3)
  }

  def getPrettyFormula(formula: Formula, nodes: Boolean): String = {
    formula.getClauses.map { c =>
      "[" + getPrettyClause(c, nodes) + "] ∧\n"
    }
      .reduceLeft(_ ++ _)
      .dropRight(3)
  }

  def prettyPrintFormula(formula: Formula, nodes: Boolean): Unit = {
    println("\n\nBoolean Formula: \n" + getPrettyFormula(formula, nodes))
  }

}
