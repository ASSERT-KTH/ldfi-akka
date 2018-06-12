package ldfi.akka

import booleanformulas.BooleanFormula._

case class FailureSpec(eot: Int,
                       eff: Int,
                       maxCrashes: Int,
                       nodes: Set[Node],
                       messages: Set[Message],
                       crashes: Set[Node] = Set.empty,
                       cuts: Set[Message] = Set.empty){

  require(maxCrashes <= nodes.size, "Can't have more crashes than nodes")
  require(crashes.size <= maxCrashes, "Can't specify more than maxCrashes crashes")
  require(cuts.forall(_.time < eff), "Can't have omissions at or after the EFF")

}











