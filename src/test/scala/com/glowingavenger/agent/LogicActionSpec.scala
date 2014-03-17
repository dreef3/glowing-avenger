package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._

class LogicActionSpec extends FlatSpec with Matchers {
  behavior of "Logic Action"

  val state = BeliefState.fromBoolExp('A & 'B & ~'C)

  it should "throw an exception when effect or precondition aren't satisfiable" in {
    an [IllegalArgumentException] should be thrownBy {
      val action = LogicAction(~'A, 'A & 'B & ~'B)
    }

    an [IllegalArgumentException] should be thrownBy {
      val action = LogicAction(~'A & 'A, 'B)
    }
  }

  it should "compute it's effect and precondition attributes" in {
    val action = LogicAction('A & ~'B, 'C & 'D)
    action.attrs should contain theSameElementsAs List('A, 'B, 'C, 'D)
  }

  it should "not be applicable when precondition is not satisfied" in {
    val action = LogicAction(~'A, 'A)
    action.applicableIn(state) shouldBe false
  }

  it should "be applicable when precondition is satisfied" in {
    val action = LogicAction('A, ~'A)
    action.applicableIn(state) shouldBe true
  }

  it should "result in the same state when executed in state where precondition is not satisfied" in {
    val action = LogicAction(~'A, 'A)

    action.applicableIn(state) shouldBe false
    action.result(state) shouldBe state
  }

  it should "result in the initial state with applied effect when precondition is satisfied" in {
    val action = LogicAction('A, ~'A)

    action.applicableIn(state) shouldBe true
    action.result(state) shouldBe BeliefState.fromBoolExp(~'A & 'B & ~'C)
  }
}
