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
    /*
    val senderNodeLiteral  = getSenderNode(line)
    val recipientNodeLiteral = getRecipientNode(line)
    if(!clause.literalExistsInClause(senderNodeLiteral))
      clause.addLiteralToClause(senderNodeLiteral)
    if(!clause.literalExistsInClause(recipientNodeLiteral))
      clause.addLiteralToClause(recipientNodeLiteral)
    */

    //all messages are unique
    clause.addLiteralToClause(messageLiteral)

  }

  def getSenderNode(line: Row): Node = {
    Node(line.sender, line.time)
  }
  def getRecipientNode(line: Row): Node = {
    Node(line.recipient, line.time)
  }


  def getMessage(line: Row): Message = {
    Message(line.sender, line.recipient, line.time)
  }

  def prettyPrintClause(clause: Clause): Unit = {
    var cnt = 0
    for(literal <- clause.literals){
      literal match {
        case Node(node, time) =>
          print("P(" + node + ", " )
          //print (time.year + "-" + time.month + "-" + time.day + "::" + time.hour + ":" + time.minute + ":" + time.second + ":" + time.millisecond + ")")
          print(time + ")")
        case Message(sender, recipient, time) =>
          print("M(" + sender + ", " + recipient + ", ")
          print(time + ")")
          //print (time.year + "-" + time.month + "-" + time.day + "::" + time.hour + ":" + time.minute + ":" + time.second + ":" + time.millisecond + ")")
        case _ => println("ERROR: literal not Node or Message!")
      }
      if(cnt != clause.literals.size - 1){
        print(" V ")
      }
      cnt = cnt + 1
    }
  }

  def prettyPrintFormula(formula: Formula): Unit = {
    var it = 1
    println("\n\nBoolean Formula:")
    for (clause <- formula.clauses){
      print("(")
      prettyPrintClause(clause)
      if(it != formula.clauses.size){
        print(")")
        print(" ∧\n")
      }
      it = it + 1
    }
    print (")")
  }

}

