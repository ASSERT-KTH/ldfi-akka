package BooleanFormulas




class Formula  {
  var clauses : List[Clause] = List.empty
  var literalsToId : Map[Literal, Int] = Map.empty
  var idToLiterals : Map[Int, Literal] = Map.empty
  var literalId = 1

  def addClause(clause: Clause): Unit = {
    clauses = clause :: clauses
  }

  def addLiteralToFormula(literal: Literal): Unit = {
    //check for structural equality
    if (!literalExistsInFormula(literal)) {
      //add to hash-maps to simplify sat-solving
      literalsToId += (literal -> literalId)
      idToLiterals  += (literalId -> literal)
      //increment id
      literalId = literalId + 1
    }
    println("ltoId size: " + literalsToId.size + " idToLit size: " + idToLiterals.size)
  }

  def literalExistsInFormula(literal: Literal): Boolean = {
    literalsToId.contains(literal)
  }

  def getLiteralId(literal: Literal): Int = {
    literalsToId.get(literal) match {
      case Some(id) => println("Found literal " + literal + " with id " + id); id
      case None => sys.error("LITERAL DOES NOT EXIST IN HASHMAP literalsToId")
    }
  }

  def getLiteral(literalId: Int): Literal = {
    idToLiterals.get(literalId) match {
      case Some(literal) => literal
      case None => sys.error("ID DOES NOT EXIST IN HASHMAP idToLiterals")
    }
  }

  def getAllNodes: List[Node] = {
    clauses.flatMap(c => c.literals.collect{ case n:Node => n })
  }

  def getAllMessages: List[Message] = {
    clauses.flatMap(c => c.literals.collect{ case msg:Message => msg })
  }

  def printLiteralIds(): Unit = {
    println("\nLiteralIds:\n ")
    for ((l, v) <- literalsToId) {
      printf("literal: " + l + ", :id " + v + "   ")
    }
    println("\n=========================")
  }
}


class Clause extends Formula {
  var literals : List[Literal] = List.empty

  def addLiteralToClause(literal: Literal): Unit = {
    addLiteralToFormula(literal)
    literals = literal :: literals
  }

  def literalExistsInClause(literal: Literal): Boolean = {
    literals.contains(literal)
  }

}


sealed trait Literal extends Formula

case class Node(node: String, time: String) extends Literal

case class Message(sender: String, recipient: String, time: String) extends Literal

