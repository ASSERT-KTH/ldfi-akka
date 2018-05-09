package ldfi.akka.Protocols.SimpleDeliv

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

class Node extends Actor {
  val name = self.path.name

  def receive = {
    case Broadcast(pload) =>
      //Log the payload in global logs
      logBroadcast(pload)

    case Start(Broadcast(pload)) =>
      //Broadcast this message to all neighbors
      sendBroadcast(Broadcast(pload))
      //Log the payload in global logs
      logBroadcast(pload)
      //Right now I'm just killing the Start actor after broadcasting the message.
      //TODO: Find much better way of doing this, i.e, if no messages has been sent, then finish or something
      context.system.terminate()
  }

  def sendBroadcast(broadcast: Broadcast): Unit = {
    Relations.relations.get(name) match {
      //Broadcast this payload to all neighboring actors
      case Some(neighbors: List[ActorRef]) =>
        for(neigh <- neighbors) {
            neigh ! broadcast
        }
      //Actor has no neighbors. Do nothing.
      case None => //Do nothing
    }
  }

  def logBroadcast(pload: String): Unit = {
    Logs.logs.get(name) match {
      //Actor mutates only its own logs, so no risks for race-conditions
      case Some(logs: mutable.Set[Log]) => logs += Log(pload)
      //If no logs exists yet, create log list with actor
      case None => Logs.logs.+=((name, mutable.Set(Log(pload))))
    }
  }

}

class SimpleDeliv {
  //Creating actor system
  val system = ActorSystem("system")

  //Creating nodes
  val A = system.actorOf(Node.props, "A")
  val B = system.actorOf(Node.props, "B")
  val C = system.actorOf(Node.props, "C")

  //Creating an immutable neighboring list. No neighbors can be added dynamically for now.
  val actors = List(A, B, C)

  /*
   Adding relations. All nodes are neighbors to each other.
   **Not using Akka's Broadcast method because I want to have control over which nodes can communicate.**
  */
  Relations.relations = immutable.HashMap(("A", List(B, C)), ("B", List(A, C)), ("C", List(A, B)))

  //The logs are initially empty
  Logs.logs = immutable.HashMap[String, mutable.Set[Log]]()

  def run(): Unit = {
    //Start Simulation
    A ! Start(Broadcast("Some payload"))

    //TODO: The time duration is arbitrary and needs to be changed to something more deterministic.
    Await.ready(system.whenTerminated, Duration(5, TimeUnit.SECONDS))
  }

  //This is just hard-coded correct for now as it complicates things.
  //Verifying the pre-condition. Basically if a log exists for a node, then that node is not crashed.
  //TODO: Remove hard-coded logic
  //Maybe something like: require !crashes:ListBuffer[ActorRef].contains(node)
  def verifyPre(): Unit = {
    require(verifyLog("", "") && verifyNotInCrash(""))
    def verifyLog(pload: String, node: String): Boolean = {
      true
    }
    def verifyNotInCrash(node: String): Boolean = {
      true
    }
  }
  //Verifying the post-condition. Namely that if a log exists for a given node, then it exists for all its neighbors
  def verifyPost(): Unit = {
    println("\n\nVerifying the correctness of the run:")
    for(actor <- actors) {
      val node = actor.path.name
      Logs.logs.get(node) match {
        //This is an O(n^2) naive algorithm that can be improved a lot since all relations are transitive.
        //For instance, if I've already checked that B has logged everything that A has, then there is no need to check
        //that A has logged everything B has etc. Low priority.
        case Some(logs: mutable.Set[Log]) => logs.foreach(log => verifyNotMissingLog(log.pload, node));
        case None => //Do nothing
      }
    }
    def verifyNotMissingLog(pload: String, node: String): Unit = {
      println
    } ensuring (!missing_log(pload, node))
  }


  def verifyPostWithoutAssert(): Boolean = {
    var res = true
    for(actor <- actors) {
      val node = actor.path.name
      Logs.logs.get(node) match {
        //This is an O(n^2) naive algorithm that can be improved a lot since all relations are transitive.
        //For instance, if I've already checked that B has logged everything that A has, then there is no need to check
        //that A has logged everything B has etc. Low priority.
        case Some(logs: mutable.Set[Log]) =>
          for (log <- logs){
            println()
            if(missing_log(log.pload, node))
              res = false
          }
        case None => //Do nothing
      }
    }
    res
  }

  def missing_log(pload: String, node: String): Boolean = {
    var missing = false
    Relations.relations.get(node) match {
      case Some(neighbors: List[ActorRef]) =>
        for (neigh <- neighbors) {
          Logs.logs.get(neigh.path.name) match {
            case Some(logs: mutable.Set[Log]) =>
              //Log exists on some node and is not logged by its neighbor. Violation of post-condition.
              if (!logs.contains(Log(pload))) missing = true
              //Log is found. In agreement with post-condition.
              else println("I found the corresponding log: \"" + pload + "\" for " + node + "'s neighbor: " + neigh.path.name)
            case None =>
              //Violation of the post-condition, since no logs at all are found.
              missing = true
              println("Didn't find any logs for " + node + "'s neighbor: " + neigh.path.name)
          }
        }
      //If no neighbors, then we are not violating the post-condition.
      case None => println("Didn't find any neighbors for: " + node)
    }
    missing
  }


}

