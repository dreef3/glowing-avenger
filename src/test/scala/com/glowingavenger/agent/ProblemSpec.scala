package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._

class ProblemSpec extends FlatSpec with Matchers {
  behavior of "Problem"

  it should "validate that init, goal, actions attributes are consistent with problem attributes" in {
    an [IllegalArgumentException] shouldBe thrownBy {
      val illegal = new Problem(List('A, 'B, 'C), Nil, True, BeliefState.fromBoolExp('D), BeliefState.fromBoolExp('A))
    }
    an [IllegalArgumentException] shouldBe thrownBy {
      val illegal = new Problem(List('A, 'B, 'C), Nil, True, BeliefState.fromBoolExp('A), BeliefState.fromBoolExp('D))
    }
    an [IllegalArgumentException] shouldBe thrownBy {
      val illegal = new Problem(List('A, 'B, 'C), List(LogicAction('D, 'A)), True, BeliefState.fromBoolExp('A), BeliefState.fromBoolExp('A))
    }
  }

}
