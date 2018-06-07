package ldfi.akka.BooleanFormulas

import ldfi.akka.BooleanFormulas.BooleanFormula._
import ldfi.akka.Parser.AkkaParser._


object CNFConverter {

  def run(formattedLog: FormattedLogs, formula: Formula): Unit = {
    val clause = new Clause
    for (line <- formattedLog.rows){
      addRowToClause(clause, line)
    }
    //prettyPrintClause(clause)
    formula.addClause(clause)
  }

  def addRowToClause(clause: Clause, line: Row): Unit = {
    val messageLiteral = getMessage(line)
    val senderNodeLiteral  = getSenderNode(line)
    val recipientNodeLiteral = getRecipientNode(line)

    if(!clause.literalExistsInClause(senderNodeLiteral))
      clause.addLiteralToClause(senderNodeLiteral)

    if(!clause.literalExistsInClause(recipientNodeLiteral))
      clause.addLiteralToClause(recipientNodeLiteral)

    //all messages are unique
    clause.addLiteralToClause(messageLiteral)
  }

  def getSenderNode(line: Row): Node = Node(line.sender, line.time)

  def getRecipientNode(line: Row): Node = Node(line.recipient, line.time)

  def getMessage(line: Row): Message = Message(line.sender, line.recipient, line.time)

  def prettyPrintClause(clause: Clause): Unit = {
    val msgs = clause.getMessagesInClause
    val nodes = clause.getNodesInClause

    val prettyMsgs = (for (msg <- msgs) yield
      "M(" + msg.sender + ", " + msg.recipient + ", " + msg.time + ")" + " V ").mkString

    val prettyNodes = (for ((node, count) <- nodes.zipWithIndex) yield {
        if(count < nodes.size - 1) "P(" + node.node + ", " + node.time + ")" + " V "
        else "P(" + node.node + ", " + node.time + ")"
    }).mkString

    val prettyClause = prettyMsgs + prettyNodes
    print(prettyClause)
  }

  def prettyPrintFormula(formula: Formula): Unit = {
    println("\n\nBoolean Formula:")
    for ((clause, cnt) <- formula.clauses.zipWithIndex){
      print("(")
      prettyPrintClause(clause)
      if(cnt < formula.clauses.size - 1){
        print(") ∧\n")
      }
    }
    print (")")
  }

}

