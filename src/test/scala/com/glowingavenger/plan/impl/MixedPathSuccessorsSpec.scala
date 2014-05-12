package com.glowingavenger.plan.impl

import com.glowingavenger.plan.{ActionEdge, PlanSpec}
import com.glowingavenger.plan.model.state.BeliefState
import org.sat4j.scala.Logic._
import com.glowingavenger.plan.model.action.Question

class MixedPathSuccessorsSpec extends PlanSpec {
  trait TestEnv extends ProblemEnv with DefaultAxioms with MixedPathSuccessors {

  }

  it should "build a mixed path" in new TestEnv {
    val list = successors(initState)
    list should (be (List(
      ActionEdge(initState, BeliefState('L.? & 'S & 'B.?), actions.find(_.name == "on_light").get),
      ActionEdge(BeliefState('L.? & 'S & 'B.?), goalState, Question('L)))) or be (
      List(
        ActionEdge(initState, BeliefState('L.? & 'S & 'B.?), actions.find(_.name == "on_light").get),
        ActionEdge(BeliefState('L.? & 'S & 'B.?), goalState, Question('B)))) or be (
    // FIXME The last one is not valid
    List(
      ActionEdge(initState, BeliefState('L.? & 'S.? & 'B), Question('B)),
      ActionEdge(BeliefState('L.? & 'S.? & 'B), goalState, actions.find(_.name == "on_light").get)
    )
    ))
  }
}
