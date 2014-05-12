package com.glowingavenger.plan.impl

import com.glowingavenger.plan.{ActionEdge, PlanSpec}
import org.sat4j.scala.Logic._
import com.glowingavenger.plan.model.state.BeliefStateImplicits
import BeliefStateImplicits._

class GuaranteedPathSuccessorsSpec extends PlanSpec {
  behavior of "Guaranteed path builder"

  trait TestEnv extends ProblemEnv with DefaultAxioms with GuaranteedPathSuccessors {
  }

  it should "build a path of logic actions only" in new TestEnv {
    val list = successors(initState)
    list should have size 3
    list shouldBe List(ActionEdge(~'L & 'S.? & 'B.?, ~'L & ~'S & 'B.?, actions.find(_.name == "off_light").get),
      ActionEdge(~'L & ~'S & 'B.?, ~'L & ~'S & 'B, actions.find(_.name == "ch_bulb").get),
      ActionEdge(~'L & ~'S & 'B, 'L & 'S & 'B, actions.find(_.name == "on_light").get)
    )
  }
}

