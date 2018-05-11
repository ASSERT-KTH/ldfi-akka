package ldfi.akka.BooleanFormulas


object BooleanFormula {

  var literalsToId : Map[Literal, Int] = Map.empty
  var idToLiterals : Map[Int, Literal] = Map.empty
  var firstMessageSent : Map[String, Int] = Map.empty
  var activityTimeRange : Map[String, (Int, Int)] = Map.empty
  var literalId = 1
  var latestTime = 0

  class Formula  {
    var clauses : List[Clause] = List.empty

    def addLiteralToFormula(literal: Literal): Unit = {
      updateLatestTime(literal)
      updateSenderTime(literal)
      updateActivityMap(literal)

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

    def idExistsInLiteralsToId(id: Int): Boolean = idToLiterals.contains(id)

    def litExistsInIdToLiterals(literal: Literal): Boolean = literalsToId.contains(literal)

    def getLitIdCnt: Int = literalId

    def getLatestTime: Int = latestTime

    def getActivityTimeRange(node: String): Option[(Int, Int)] = {
      activityTimeRange.get(node) match {
        case valu @ Some(value) => valu
        case None => None
      }
    }

    def getLiteralId(literal: Literal): Int = {
      literalsToId.get(literal) match {
        case Some(id) => id
        case None => -999 //sys.error("LITERAL DOES NOT EXIST IN HASHMAP literalsToId")
      }
    }

    def getLiteral(literalId: Int): Literal = {
      idToLiterals.get(literalId) match {
        case Some(literal) => literal
        case None => sys.error("Literalid: " + literalId + ", does not exist in hashmap idToLiterals")
      }
    }

    def updateLatestTime (literal: Literal): Unit = literal match {
      case Node(_, time) => if(time > latestTime) latestTime = time
      case Message(_, _, time) => if(time > latestTime) latestTime = time
    }

    def updateSenderTime(literal: Literal): Unit = {
      literal match {
        case n @ Node(id, time) => //DO NOTHING
        case m @ Message(sender, recipient, time) =>
          firstMessageSent.get(sender) match {
            case Some(storedtime) if storedtime > time => firstMessageSent + (sender -> time)
            case Some(storedtime) if storedtime <= time => // Do nothing
            case None => firstMessageSent += (sender -> time)
          }
      }
    }

    def updateActivityMap(literal: Literal): Unit = {
      literal match {
        case Node(id, currTime) => updateActivityMapHelper(id, currTime)
        case Message(sender, recipient, currTime) =>
          updateActivityMapHelper(sender, currTime)
          updateActivityMapHelper(recipient, currTime)
      }

      def updateActivityMapHelper(node: String, currTime: Int): Unit = {
        activityTimeRange.get(node) match {
          case Some((firstTime, lastTime)) =>
            if(currTime < firstTime)
              activityTimeRange + (node -> (currTime, lastTime))
            if(currTime > lastTime)
              activityTimeRange + (node -> (firstTime, lastTime))
          case None =>
            activityTimeRange += (node -> (currTime, currTime))
        }
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

  final case class Node(node: String, time: Int) extends Literal

  final case class Message(sender: String, recipient: String, time: Int) extends Literal
}


