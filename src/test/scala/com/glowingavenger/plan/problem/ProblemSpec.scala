package com.glowingavenger.plan.problem

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._
import com.glowingavenger.plan.problem._

class ProblemSpec extends FlatSpec with Matchers {
  behavior of "Problem"

  val defaultHeruistic = (i: BeliefState, g: BeliefState) => 0

  it should "validate that init, goal, actions attributes are consistent with problem attributes" in {
    an [IllegalArgumentException] shouldBe thrownBy {
      val illegal = new PDDLProblem(List('A, 'B, 'C), Nil, True, BeliefState.fromBoolExp('D), BeliefState.fromBoolExp('A), defaultHeruistic)
    }
    an [IllegalArgumentException] shouldBe thrownBy {
      val illegal = new PDDLProblem(List('A, 'B, 'C), Nil, True, BeliefState.fromBoolExp('A), BeliefState.fromBoolExp('D), defaultHeruistic)
    }
    an [IllegalArgumentException] shouldBe thrownBy {
      val illegal = new PDDLProblem(List('A, 'B, 'C), List(LogicAction('D, 'A)), True, BeliefState.fromBoolExp('A), BeliefState.fromBoolExp('A), defaultHeruistic)
    }
  }

}
