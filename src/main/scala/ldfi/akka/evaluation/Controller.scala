package ldfi.akka.evaluation

import akka.actor._
import ldfi.akka.booleanformulas._

object Controller {
  private var infoHolder: infoHolder = new infoHolder
  private var injections: Set[Literal] = Set.empty

  private class infoHolder {
    private var previousSender: String = _
    private var time: Int = 0

    def tickClock(): Unit = time = time + 1
    def getTime: Int = time
    def updateInfo(curSen: String): Unit = previousSender = curSen
    def getPrev: String = previousSender

  }

  def greenLight(sender: String, recipient: String, message: Any): Boolean = {

    val time = manageClock(sender, recipient)

    //we do not give greenLight if message is cut or node is crashed
    val greenLight = !isInjected(sender, recipient, injections, time, message)
    greenLight
  }

  def isInjected(sen: String, rec: String, injections: Set[Literal], time: Int, message: Any): Boolean = {
    val msg = MessageLit(sen, rec, time, message.toString)
    val msgcut = injections.contains(msg)

    //nodes crashes if current time is greater or equal to injection time
    val senderCrashed = injections.collect{ case n @ Node(name, tme) if sen == name && tme <= time => n }.nonEmpty
    val recipientCrashed = injections.collect{ case n @ Node(name, tme) if rec == name && tme <= time => n }.nonEmpty

    //We send OK if the message is not omitted and neither node is crashed
    val isInjected = msgcut || senderCrashed || recipientCrashed
    isInjected
  }


  def manageClock(curSen: String, curRec: String): Int = {
    val prevSen = infoHolder.getPrev
    //increment clock if new sender
    if(curSen != prevSen)
      infoHolder.tickClock()

    infoHolder.updateInfo(curSen)
    infoHolder.getTime
  }

  def setInjections(injns: Set[Literal]): Unit = injections = injns

  def reset(): Unit = infoHolder = new infoHolder

}


