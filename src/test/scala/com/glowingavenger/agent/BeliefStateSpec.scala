package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._
import com.glowingavenger.agent.problem.BeliefState

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

  it should "not be included if any attribute is not present" in {
    val inner = new BeliefState(Map('A -> Some(true), 'B -> None))
    val outer = new BeliefState(Map('A -> Some(true), 'C -> Some(false)))
    outer includes inner shouldBe false
  }

  it should "not be included if any attribute has different value" in {
    val inner = new BeliefState(Map('A -> Some(true), 'B -> Some(true)))
    val outer = new BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    outer includes inner shouldBe false
  }

  it should "not be included if inner attribute is unknown" in {
    val inner = new BeliefState(Map('A -> Some(true), 'B -> None))
    val outer = new BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    outer includes inner shouldBe false
  }

  it should "be included if outer attribute is unknown" in {
    val inner = new BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    val outer = new BeliefState(Map('A -> Some(true), 'B -> None))
    outer includes inner shouldBe true
  }

  it should "not be included if all attributes have same values" in {
    val inner = new BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    val outer = new BeliefState(Map('A -> Some(true), 'B -> Some(false), 'C -> Some(true)))
    outer includes inner shouldBe true
  }
}
