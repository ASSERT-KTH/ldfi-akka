package ldfi.akka.booleanformulas

class Formula {

  //helper fields for later SAT-solving
  var literalsToId: Map[Literal, Int] = Map.empty
  var idToLiterals: Map[Int, Literal] = Map.empty
  var firstMessageSent: Map[String, Int] = Map.empty
  var activityTimeRange: Map[String, (Int, Int)] = Map.empty
  var literalId = 1
  var latestTime = 0

  var clauses: List[Clause] = List.empty

  def addLiteralToFormula(literal: Literal): Unit = {
    updateLatestTime(literal)
    updateSenderTime(literal)
    updateActivityMap(literal)

    if (!literalExistsInFormula(literal)) {
      literalsToId += (literal -> literalId)
      idToLiterals += (literalId -> literal)
      literalId = literalId + 1
    }
  }

  def addClause(clause: Clause): Unit = clauses = clause :: clauses

  def getAllLiterals: Set[Literal] =
    clauses.flatMap(c => c.getLiteralsInClause).toSet

  def getAllNodes: Set[Node] =
    clauses.flatMap(c => c.getNodesInClause).sortWith(_.time > _.time).toSet

  def getAllMessages: List[MessageLit] =
    clauses
      .flatMap(c => c.getMessagesInClause)
      .sortWith(_.time > _.time)
      .distinct

  def literalExistsInFormula(literal: Literal): Boolean =
    literalsToId.contains(literal)

  def idExistsInLiteralsToId(id: Int): Boolean = idToLiterals.contains(id)

  def litExistsInIdToLiterals(literal: Literal): Boolean =
    literalsToId.contains(literal)

  def getLitIdCnt: Int = literalId

  def getLatestTime: Int = latestTime

  def getActivityTimeRange(node: String): (Int, Int) = {
    activityTimeRange.get(node) match {
      case Some(actRange) => actRange
      case None           => sys.error("Solver: Node doesn't have any activity. " + node)
    }
  }

  def getLiteralId(literal: Literal): Int = {
    literalsToId.get(literal) match {
      case Some(id) => id
      case None =>
        sys.error(
          "Literal: " + literal + ", does not exist in hashmap literalsToId")
    }
  }

  def getLiteral(literalId: Int): Literal = {
    idToLiterals.get(literalId) match {
      case Some(literal) => literal
      case None =>
        sys.error(
          "Literalid: " + literalId + ", does not exist in hashmap idToLiterals")
    }
  }

  def updateLatestTime(literal: Literal): Unit = literal match {
    case Node(_, time)             => if (time > latestTime) latestTime = time
    case MessageLit(_, _, time, _) => if (time > latestTime) latestTime = time
  }

  def updateSenderTime(literal: Literal): Unit = {
    literal match {
      case n @ Node(id, time) => //DO NOTHING
      case m @ MessageLit(sender, recipient, time, _) =>
        firstMessageSent.get(sender) match {
          case Some(storedtime) if time > storedtime =>
            firstMessageSent += (sender -> time)
          case Some(storedtime) if time <= storedtime => // Do nothing
          case None                                   => firstMessageSent += (sender -> time)
        }
    }
  }

  def updateActivityMap(literal: Literal): Unit = {
    literal match {
      case Node(id, currTime) => updateActivityMapHelper(id, currTime)
      case MessageLit(sender, recipient, currTime, _) =>
        updateActivityMapHelper(sender, currTime)
        updateActivityMapHelper(recipient, currTime)
    }

    def updateActivityMapHelper(node: String, currTime: Int): Unit = {
      activityTimeRange.get(node) match {
        case Some((firstTime, lastTime)) =>
          if (currTime < firstTime)
            activityTimeRange += (node -> (currTime, lastTime))
          if (currTime > lastTime)
            activityTimeRange += (node -> (firstTime, currTime))
        case None =>
          activityTimeRange += (node -> (currTime, currTime))

      }
    }
  }

}

class Clause(formula: Formula) {
  var literals: List[Literal] = List.empty

  def addLiteralToClause(literal: Literal): Unit = {
    formula.addLiteralToFormula(literal)
    literals = literal :: literals
  }

  def getLiteralsInClause: List[Literal] = {
    def getTimeFromLit(lit: Literal): Int = lit match {
      case m: MessageLit => m.time
      case n: Node       => n.time
    }
    literals.sortWith(getTimeFromLit(_) > getTimeFromLit(_))
  }

  def getMessagesInClause: List[MessageLit] =
    literals.collect { case m: MessageLit => m }.sortWith(_.time > _.time)

  def getNodesInClause: Set[Node] =
    literals.collect { case n: Node => n }.sortWith(_.time > _.time).toSet

  def literalExistsInClause(literal: Literal): Boolean =
    literals.contains(literal)

}

sealed trait Literal extends Formula

final case class Node(node: String, time: Int) extends Literal

final case class MessageLit(sender: String,
                            recipient: String,
                            time: Int,
                            message: String)
  extends Literal