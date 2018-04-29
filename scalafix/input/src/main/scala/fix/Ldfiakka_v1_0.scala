/*
rule = "class:fix.Ldfiakka_v1_0"
*/
package fix

import java.util.concurrent.TimeUnit

import akka.actor._
import akka.event._

import scala.collection.{mutable, _}
import scala.concurrent.Await
import scala.concurrent.duration.Duration



case class Start(bcast: Broadcast)
case class Broadcast(pload: String)
case class Log(pload: String)

//The global neighbor map of all nodes
object Relations {
  var relations: immutable.HashMap[String, List[ActorRef]] = immutable.HashMap[String, List[ActorRef]]()
}

//Global logs
object Logs {
  //Hashmap is mutable, but no risk of race-condition because each actor only mutate its own logs.
  var logs = immutable.HashMap[String, mutable.Set[Log]]()
}

//Propsfactory. Do not have any constructor parameters for now, but still best practice.
object Node {
  def props: Props = Props(new Node())
}

class Node extends Actor with ActorLogging {
  val name = self.path.name
  def receive = {
    case Broadcast(pload) =>
      logBroadcast(pload)
    case Start(Broadcast(pload)) =>
      sendBroadcast(Broadcast(pload))
      logBroadcast(pload)
      context.system.terminate()
  }
  def sendBroadcast(broadcast: Broadcast): Unit = {
    Relations.relations.get(name) match {
      case Some(neighbors: List[ActorRef]) =>
        for(neigh <- neighbors) {
          neigh ! broadcast
        }
      case None =>
    }
  }
  def logBroadcast(pload: String): Unit = {
    Logs.logs.get(name) match {
      case Some(logs: mutable.Set[Log]) => logs += Log(pload)
      case None => Logs.logs.+=((name, mutable.Set(Log(pload))))
    }
  }
}
