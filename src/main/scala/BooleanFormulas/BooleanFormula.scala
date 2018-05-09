package BooleanFormulas


object BooleanFormula {
  var literalsToId : Map[Literal, Int] = Map.empty
  var idToLiterals : Map[Int, Literal] = Map.empty
  var literalId = 1

  class Formula  {
    var clauses : List[Clause] = List.empty

    def addLiteralToFormula(literal: Literal): Unit = {
      if (!literalExistsInFormula(literal)) {
        literalsToId += (literal -> literalId)
        idToLiterals  += (literalId -> literal)
        literalId = literalId + 1
      }
    }

    def addClause(clause: Clause): Unit = clauses = clause :: clauses

    def getAllLiterals: List[Literal] = clauses.flatMap(c => c.literals)

    def getAllNodes: List[Node] = clauses.flatMap(c => c.literals.collect{ case n:Node => n })

    def getAllMessages: List[Message] = clauses.flatMap(c => c.literals.collect{ case msg:Message => msg })

    def literalExistsInFormula(literal: Literal): Boolean = literalsToId.contains(literal)

    def getLiteralId(literal: Literal): Int = {
      literalsToId.get(literal) match {
        case Some(id) => id
        case None => sys.error("LITERAL DOES NOT EXIST IN HASHMAP literalsToId")
      }
    }

    def getLiteral(literalId: Int): Literal = {
      idToLiterals.get(literalId) match {
        case Some(literal) => literal
        case None => sys.error("ID DOES NOT EXIST IN HASHMAP idToLiterals")
      }
    }

  }


  class Clause extends Formula {
    var literals : List[Literal] = List.empty

    def addLiteralToClause(literal: Literal): Unit = {
      addLiteralToFormula(literal)
      literals = literal :: literals
    }

    def literalExistsInClause(literal: Literal): Boolean = literals.contains(literal)

  }


  sealed trait Literal extends Formula

  case class Node(node: String, time: String) extends Literal

  case class Message(sender: String, recipient: String, time: String) extends Literal
}


