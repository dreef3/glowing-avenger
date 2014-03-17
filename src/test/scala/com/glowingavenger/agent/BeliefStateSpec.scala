package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._

class BeliefStateSpec extends FlatSpec with Matchers {
  behavior of "Belief State"

  it should "be convertible to boolean expression" in {
    val state = new BeliefState(Map('A -> None, 'B -> Some(true), 'C -> Some(false)))
    state.asBoolExp shouldBe 'B & ~'C
  }

  it should "be convertible from simple boolean expression" in {
    val clause = 'B  & ~'C
    BeliefState.fromBoolExp(clause) shouldBe new BeliefState(Map('B -> Some(true), 'C -> Some(false)))
  }

  it should "be convertible from boolean expression with extra attributes" in {
    val attrs = List('A, 'B, 'D)
    val clause = 'B & ~'C
    BeliefState.fromBoolExp(clause, attrs) shouldBe new BeliefState(Map('A -> None, 'B -> Some(true), 'C -> Some(false), 'D -> None))
  }
}
