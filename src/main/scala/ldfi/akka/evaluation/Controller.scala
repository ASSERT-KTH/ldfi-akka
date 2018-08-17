package ldfi.akka.evaluation

import ldfi.akka.booleanformulas._

object Controller {
  private var injections: Set[Literal] = Set.empty

  object Clock {
    private var time: Int = 0

    def tick(): Unit = time = time + 1
    def getTime: Int = time
    def reset(): Unit = time = 0
  }

  def greenLight(sender: String, recipient: String, message: Any): Boolean = {

    val curTime = Clock.getTime
    //we do not give greenLight if message is cut or node is crashed
    val greenLight =
      !isInjected(sender, recipient, injections, curTime + 1, message)

    if (greenLight) {
      //we only tick clock if message is not interjected, because it is not going to be witnessed by the parser.
      Clock.tick()
    }

    greenLight
  }

  def isInjected(sen: String,
                 rec: String,
                 injections: Set[Literal],
                 time: Int,
                 message: Any): Boolean = {
    val msg = MessageLit(sen, rec, time, message.toString)
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
    isInjected
  }

  def setInjections(injns: Set[Literal]): Unit = injections = injns

  def reset(): Unit = Clock.reset()

}
