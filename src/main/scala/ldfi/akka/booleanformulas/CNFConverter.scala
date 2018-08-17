package ldfi.akka.booleanformulas

import ldfi.akka.parser.AkkaParser._

object CNFConverter {

  def run(formattedLog: FormattedLogs, formula: Formula): Unit = {
    val clause = new Clause(formula)
    for (line <- formattedLog.rows) {
      addRowToClause(clause, line)
    }
    //prettyPrintClause(clause)
    formula.addClause(clause)
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
    MessageLit(line.sender, line.recipient, line.time, line.message)

  def prettyPrintClause(clause: Clause): Unit = {
    for ((literal, cnt) <- clause.literals.zipWithIndex) {
      literal match {
        case Node(node, time) => print("P(" + node + ", " + time + ")")
        case MessageLit(sender, recipient, time, message) =>
          print(
            "M(" + sender + ", " + recipient + ", " + time + ", " + message + ")")
        case _ =>
          println(
            "ERROR in CNFConverter.prettyPrintClause: literal not Node or Message!")
      }
      if (cnt != clause.literals.size - 1) {
        print(" V ")
      }
    }
  }

  def prettyPrintFormula(formula: Formula): Unit = {
    println("\n\nBoolean Formula:")
    for ((clause, cnt) <- formula.clauses.zipWithIndex) {
      print("(")
      prettyPrintClause(clause)
      if (cnt < formula.clauses.size - 1) {
        print(") ∧\n")
      }
    }
    print(")")
  }

}
