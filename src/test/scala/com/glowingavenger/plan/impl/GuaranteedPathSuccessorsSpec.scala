package com.glowingavenger.plan.impl

import com.glowingavenger.plan.model.{BeliefState, Problem}
import com.glowingavenger.plan.{ActionEdge, PlanSpec}
import org.sat4j.scala.Logic._
import com.glowingavenger.plan.model.BeliefStateImplicits._
import com.glowingavenger.plan.model.action.LogicAction

class GuaranteedPathSuccessorsSpec extends PlanSpec {
  behavior of "Guaranteed path builder"

  trait TestEnv extends GuaranteedPathSuccessors with DefaultAxioms with ProblemEnv {
    val initState = BeliefState(Map('L -> Some(false), 'B -> None, 'S -> None))
  }

  /*
List((-L B? S?) -> (LogicAction(('S | ~'S), ~'S, off_light)) -> (-L B? -S), (-L B? -S) -> (LogicAction((('B | ~'B) & ~'S), 'B, ch_bulb)) -> (-L B -S), (-L B -S) -> (LogicAction(('S | ~'S), ('S & ('L | ~'L)), on_light)) -> (L B S)) was not equal to List((S? -L B?) -> (LogicAction(('S | ~'S), ~'S, off_light)) -> (-S -L B), (S? -L B?) -> (LogicAction((('B | ~'B) & ~'S), 'B, ch_bulb)) -> (-S -L B), (S? -L B?) -> (LogicAction(('S | ~'S), ('S & ('L | ~'L)), on_light)) -> (-S -L B))
org.scalatest.exceptions.TestFailedException: List((-L B? S?) -> (LogicAction(('S | ~'S), ~'S, off_light)) -> (-L B? -S), (-L B? -S) -> (LogicAction((('B | ~'B) & ~'S), 'B, ch_bulb)) -> (-L B -S), (-L B -S) -> (LogicAction(('S | ~'S), ('S & ('L | ~'L)), on_light)) -> (L B S)) was not equal to List((S? -L B?) -> (LogicAction(('S | ~'S), ~'S, off_light)) -> (-S -L B), (S? -L B?) -> (LogicAction((('B | ~'B) & ~'S), 'B, ch_bulb)) -> (-S -L B), (S? -L B?) -> (LogicAction(('S | ~'S), ('S & ('L | ~'L)), on_light)) -> (-S -L B))

   */

  it should "build a path of logic actions only" in new TestEnv {
    val list = successors(initState)
    list should have size 3
    list shouldBe List(ActionEdge(~'L & 'S.? & 'B.?, ~'L & ~'S & 'B.?, actions.find(_.name == "off_light").get),
      ActionEdge(~'L & ~'S & 'B.?, ~'L & ~'S & 'B, actions.find(_.name == "ch_bulb").get),
      ActionEdge(~'L & ~'S & 'B, 'L & 'S & 'B, actions.find(_.name == "on_light").get)
    )
  }
}

