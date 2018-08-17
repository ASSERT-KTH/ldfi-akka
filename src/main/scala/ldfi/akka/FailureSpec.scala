package ldfi.akka

import ldfi.akka.booleanformulas._

case class FailureSpec(eot: Int,
                       eff: Int,
                       maxCrashes: Int,
                       nodes: Set[Node],
                       messages: Set[MessageLit],
                       crashes: Set[Node] = Set.empty,
                       cuts: Set[MessageLit] = Set.empty) {

  require(maxCrashes <= nodes.size, "Can't have more crashes than nodes")
  require(crashes.size <= maxCrashes,
          "Can't specify more than maxCrashes crashes")
  require(cuts.forall(eot - _.time < eff),
          "Can't have omissions at or after the EFF")

}
