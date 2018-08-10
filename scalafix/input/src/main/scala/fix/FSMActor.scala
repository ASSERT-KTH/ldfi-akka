/*
rule = "class:fix.Ldfiakka_v1_0"
*/
package fix

import akka.actor._
import akka.actor.FSM._
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait State
case class Activity(state: String)
case object Busy extends State
case object Ready extends State


class FSMActor extends Actor with FSM[State, Activity] {

  startWith(Ready, Activity("Ready"))

  //case 1
  when(Ready) {
    case ev @ Event(_, _)  =>
      goto(Busy)
      goto(Busy)
  }

  //case 2
  when(Ready) {
    case ev @ Event(_, _) =>
      goto(Busy)
  }

  //case 3
  when(Ready) {
    case Event(_, _)  =>
      goto(Busy)
  }

  //case 4
  when(Ready) {
    case Event(_, _)  =>
      goto(Busy)
      goto(Busy)
  }

  //case 5
  when(Ready) {
    case _ =>
      goto(Busy)
  }

  //case 6
  when(Ready) {
    case _ =>
      goto(Busy)
      goto(Busy)
  }

  //case 7
  whenUnhandled {
    case _ =>
      goto(Busy)
  }

  onTransition {
    case Ready -> Busy =>
      self ! Ready
  }

}

class FSMTest {

  def main(args: Array[String]): Unit = {
    val system: ActorSystem = ActorSystem("system")
    val FSMActor: ActorRef = system.actorOf(Props[FSMActor], "FSM")
  }

}
