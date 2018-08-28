package ldfi.akka.evaluation

import ldfi.akka.booleanformulas._

object Controller {

  private var injections: Set[Literal] = Set.empty
  private var formula: Formula = new Formula
  private var clauseClock: Map[Int, Int] = Map.empty

  def greenLight(sender: String, recipient: String, message: Any): Boolean = {

    if (injections.nonEmpty) {
      clauseClock = manageClock(sender,
                                recipient,
                                message.toString,
                                formula.clauses,
                                clauseClock)

      val greenLight = clauseClock.forall {
        case (cId, time) =>
          val (injected, msgcut) =
            isInjected(sender, recipient, injections, time, message)

          //If message has been injected, remove it from the injections set
          if (msgcut) {
            val msg = MessageLit(sender, recipient, time)(message.toString)
            this.injections = injections.filter {
              case m: MessageLit => m != msg
              case n: Node       => true
            }
          }
          !injected
      }
      greenLight
    } else {
      true
    }

  }

  def isInjected(sen: String,
                 rec: String,
                 injections: Set[Literal],
                 time: Int,
                 message: Any): (Boolean, Boolean) = {

    val msgcut =
      injections.collect {
        case m: MessageLit
            if sen == m.sender && rec == m.recipient && time == m.time =>
          m
      }.nonEmpty

    //nodes crashes if current time is greater or equal to injection time
    val senderCrashed = injections.collect {
      case n @ Node(name, tme) if sen == name && tme <= time => n
    }.nonEmpty
    val recipientCrashed = injections.collect {
      case n @ Node(name, tme) if rec == name && tme <= time => n
    }.nonEmpty

    //We send OK if the message is not omitted and neither node is crashed
    val isInjected = msgcut || senderCrashed || recipientCrashed
    (isInjected, msgcut)
  }

  def manageClock(sender: String,
                  recipient: String,
                  message: String,
                  clauses: List[Clause],
                  clauseClock: Map[Int, Int]): Map[Int, Int] = {
    clauses
      .map { c =>
        val time = clauseClock.get(c.getId) match {
          case Some(t) => t
          case None =>
            sys.error("Controller: could not find clauseId in clauseClock.")
        }

        val potentialTime = time + 1
        val msg = MessageLit(sender, recipient, potentialTime)(message)

        if (c.literalExistsInClause(msg)) {
          Map(c.getId -> potentialTime)
        } else {
          //get the first message where the sender is active and update clause clock to that time if such a message exists
          val updatedTime = c.getMessagesInClauseAsc.collectFirst {
            case msg: MessageLit if msg.sender == sender && msg.time > time =>
              msg.time
          }
          updatedTime match {
            case Some(t) => Map(c.getId -> t)
            case None    => Map(c.getId -> time)
          }
        }

      }
      .reduceLeft(_ ++ _)

  }

  def setFormula(form: Formula): Unit = {
    formula = form
    form.clauses.foreach { c =>
      clauseClock += c.getId -> 0
    }
  }

  def setInjections(injns: Set[Literal]): Unit = injections = injns

}
