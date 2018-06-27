package ldfi.akka.evaluation

import java.util.concurrent.ConcurrentLinkedQueue

import akka.actor._
import akka.dispatch.Envelope
import ldfi.akka.booleanformulas._
import ldfi.akka.evaluation.Controller.SuperEnvelope

import scala.collection.mutable

object Controller {

  private var infoHolder: ControllerInfoHolder = new ControllerInfoHolder

  //Behöver något som säger att ActorSystemet är klart
  def dispatchQueue(queue: ConcurrentLinkedQueue[Envelope],
                    scheduleMap: Map[Message, Int],
                    forcedSchedule: mutable.Queue[SuperEnvelope],
                    newMessages: mutable.Queue[SuperEnvelope]): Unit = {

    //sort forcedSchedule according to initialScheduleMap
    val sortedSchedule = forcedSchedule
      .map { x =>
        scheduleMap.get(x.message) match {
          case Some(place) => (x, place)
          case None => sys.error("Dispatchqueue: could not find message in initial schedule map")
        }
      }
      .sortWith(_._2 < _._2)
      .map(x => x._1)

    //append newMessages to tail
    val newSchedule = sortedSchedule ++ newMessages

    //dispatch messages
    newSchedule.foreach { superEnv => queue.offer(superEnv.handle) }

  }

  def handleSchedule(receiver: ActorRef,
                     handle: Envelope,
                     queue: ConcurrentLinkedQueue[Envelope]): Unit = {

    val initialScheduleMap = infoHolder.getInitialScheduleMap
    val forcedSchedule = infoHolder.getForcedSchedule
    val newMessages = infoHolder.getNewMessages

    val sndName = handle.sender.path.name
    val rcpName = receiver.path.name

    val currMsg = Message(sndName, rcpName, infoHolder.getTime)

    //check if message exists in initial schedule
    initialScheduleMap.get(currMsg) match {
      case Some(_) => forcedSchedule += SuperEnvelope(currMsg, handle)
      case None => newMessages += SuperEnvelope(currMsg, handle)
    }
  }

  def greenLight(sender: ActorRef, recipient: ActorRef): Boolean = {
    val sndName = sender.path.name
    val rcpName = recipient.path.name
    val time = manageClock(sndName, rcpName)

    val injections = infoHolder.getInjections

    //we do not give greenLight if message is cut or node is crashed
    val greenLight = !isInjected(sndName, rcpName, injections, time)
    greenLight
  }

  def isInjected(sen: String, rec: String, injections: List[Literal], time: Int): Boolean = {
    val msg = Message(sen, rec, time)
    val msgCut = injections.contains(msg)

    //nodes crashes if current time is greater or equal to injection time
    val senderCrashed = injections.collect {
      case n @ Node(name, tme) if sen == name && tme <= time => n
    }.nonEmpty

    val recipientCrashed = injections.collect {
      case n @ Node(name, tme) if rec == name && tme <= time => n
    }.nonEmpty

    //We send OK if the message is not omitted and neither node is crashed
    val isInjected = msgCut || senderCrashed || recipientCrashed
    isInjected
  }

  def manageClock(curSen: String, curRec: String): Int = {
    val prevSen = infoHolder.getPrev
    //increment clock if new sender
    if (curSen != prevSen)
      infoHolder.tickClock()

    infoHolder.updatePreviousSender(curSen)
    infoHolder.getTime
  }

  def reset(): Unit = infoHolder = new ControllerInfoHolder

  def setInjections(injns: Set[Literal]): Unit ={
    infoHolder.setInjections(injns)
  }

  def setInitialScheduling(initSched: Set[Message]): Unit = {
    //Make position map out of initial schedule
    val initSchedMap = getScheduleMap(initSched.toList, 1)
    infoHolder.setInitialScheduleMap(initSchedMap)
  }

  def getScheduleMap(initSched: List[Message], cnt: Int): Map[Message, Int] = initSched match {
    case Nil => Map()
    case head :: tail => Map(head -> cnt) ++ getScheduleMap(tail, cnt + 1)
  }

  final case class SuperEnvelope(message: Message, handle: Envelope)

}

class ControllerInfoHolder {

  private var injections: Set[Literal] = Set.empty

  private var initialScheduleMap: Map[Message, Int] = Map.empty
  private var forcedSchedule: mutable.Queue[SuperEnvelope] = mutable.Queue.empty
  private var newMessages: mutable.Queue[SuperEnvelope] = mutable.Queue.empty

  private var previousSender: String = _
  private var time: Int = 0


  def getInjections: List[Literal] = injections.toList

  def setInjections(injns: Set[Literal]): Unit  = injections = injns

  def getInitialScheduleMap: Map[Message, Int] = initialScheduleMap

  def setInitialScheduleMap(initSched: Map[Message, Int]): Unit  = initialScheduleMap = initSched

  def getForcedSchedule: mutable.Queue[SuperEnvelope] = forcedSchedule

  def getNewMessages: mutable.Queue[SuperEnvelope] = newMessages

  def getTime: Int = time

  def tickClock(): Unit = time = time + 1

  def updatePreviousSender(curSen: String): Unit = previousSender = curSen

  def getPrev: String = previousSender


}


