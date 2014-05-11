package com.glowingavenger.plan.model.action

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._
import com.glowingavenger.plan.model.state.BeliefState

class LogicActionSpec extends FlatSpec with Matchers {
  behavior of "Logic Action"

  val state = BeliefState('A & 'B & ~'C)

  val satState = BeliefState(~'A)

  val action = LogicAction(~'A, 'A)

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
    action.applicableIn(state) shouldBe false
  }

  it should "be applicable when precondition is satisfied" in {
    action.applicableIn(satState) shouldBe true
  }

  it should "result in the same state when executed in state where precondition is not satisfied" in {
    action.applicableIn(state) shouldBe false
    action.result(state) shouldBe state
  }

  it should "result in the initial state with applied effect when precondition is satisfied" in {
    action.applicableIn(satState) shouldBe true
    action.result(satState) shouldBe BeliefState('A)
  }

  it should "treat unknown predicates both in precondition and given state" in {
    action.applicableIn(BeliefState('A.?)) shouldBe false
    LogicAction('A?, 'A) applicableIn BeliefState('A?) shouldBe true
    LogicAction('A?, 'A) applicableIn BeliefState('A) shouldBe true
    LogicAction('A?, 'A) applicableIn BeliefState(~'A) shouldBe true

    LogicAction(('A?) & 'B, 'A) applicableIn BeliefState(('A?) & 'B) shouldBe true
    LogicAction(('A?) & ~'B, 'A) applicableIn BeliefState('A & ~'B) shouldBe true
    LogicAction(('A?) & ('B?), 'A) applicableIn BeliefState(~'A & ('B?)) shouldBe true
  }

  it should "result known predicate in unknown one when effect contains it" in {
    LogicAction('A, 'A?) result BeliefState('A) shouldBe BeliefState('A?)
    LogicAction('A, ('A?) & 'B) result BeliefState('A) shouldBe BeliefState(('A?) & 'B)
  }

  it should "treat unset predicates as unknown both in precondition and given state" in {
    LogicAction(~'A, 'A) applicableIn BeliefState('B) shouldBe false
    LogicAction('A?, 'A) applicableIn BeliefState('B) shouldBe true
    LogicAction(('A?) & ('B?), 'A) applicableIn BeliefState('A) shouldBe true
    LogicAction(('A?) & ('B?), 'A) applicableIn BeliefState(~'A) shouldBe true
  }
}
