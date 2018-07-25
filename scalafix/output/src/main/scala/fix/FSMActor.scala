package fix

import akka.actor._
import akka.actor.FSM._
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.event._
import akka.actor.ActorLogging
import akka.testkit.CallingThreadDispatcher
import ldfi.akka.evaluation.Controller

trait State
case class Activity(state: String)
case object Busy extends State
case object Ready extends State


class FSMActor extends Actor with FSM[State, Activity] with ActorLogging {

  startWith(Ready, Activity("Ready"))

  //case 1
  when(Ready) {
    case ev @ Event(_, _)  =>
      log.debug(" received handled message " + ev + " from " + sender())
goto(Busy)
      goto(Busy)
  }

  //case 2
  when(Ready) {
    case ev @ Event(_, _) =>
      log.debug(" received handled message " + ev + " from " + sender())
goto(Busy)
  }

  //case 3
  when(Ready) {
    case ev @ Event(_, _)  =>
      log.debug(" received handled message " + ev + " from " + sender())
goto(Busy)
  }

  //case 4
  when(Ready) {
    case ev @ Event(_, _)  =>
      log.debug(" received handled message " + ev + " from " + sender())
goto(Busy)
      goto(Busy)
  }

  //case 5
  when(Ready) {
    case ev =>
      log.debug(" received handled message " + ev + " from " + sender())
goto(Busy)
  }

  //case 6
  when(Ready) {
    case ev =>
      log.debug(" received handled message " + ev + " from " + sender())
goto(Busy)
      goto(Busy)
  }

  //case 7
  whenUnhandled {
    case ev =>
      log.debug(" received handled message " + ev + " from " + sender())
goto(Busy)
  }

  onTransition {
    case Ready -> Busy =>
      if (Controller.greenLight(self.path.name, self.path.name, Ready)) self ! Ready
  }

}

class FSMTest {

  def main(args: Array[String]): Unit = {
    val system : ActorSystem = ActorSystem("system")
    val FSMActor: ActorRef = system.actorOf(Props[FSMActor].withDispatcher(CallingThreadDispatcher.Id), "FSM")
  }

}
