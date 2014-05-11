package com.glowingavenger.plan.model.state

import org.scalatest.{Matchers, FlatSpec}
import org.sat4j.scala.Logic._

class BeliefStateSpec extends FlatSpec with Matchers {
  behavior of "Belief State"

  it should "be convertible to boolean expression" in {
    val state = BeliefState(Map('A -> None, 'B -> Some(true), 'C -> Some(false)))
    state.toExpr shouldBe ('A?) & 'B & ~'C
  }

  it should "be convertible from simple boolean expression" in {
    val clause = 'B  & ~'C
    BeliefState(clause) shouldBe BeliefState(Map('B -> Some(true), 'C -> Some(false)))
  }

  it should "be convertible from boolean expression with extra predicates" in {
    val attrs = List('A, 'B, 'D)
    val clause = 'B & ~'C
    BeliefState(clause, attrs) shouldBe BeliefState(Map('A -> None, 'B -> Some(true), 'C -> Some(false), 'D -> None))
  }

  it should "be convertible from a boolean expression with unknown predicates" in {
    val clause = ('A?) & 'B & ~'C
    BeliefState(clause) shouldBe BeliefState(Map('A -> None, 'B -> Some(true), 'C -> Some(false)))
  }

  it should "not be included if any predicate has different value" in {
    val inner = BeliefState(Map('A -> Some(true), 'B -> Some(true)))
    val outer = BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    outer includes inner shouldBe false
  }

  it should "not be included if inner predicate is unknown" in {
    val inner = BeliefState(Map('A -> Some(true), 'B -> None))
    val outer = BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    outer includes inner shouldBe false
  }

  it should "be included if outer predicate is unknown" in {
    val inner = BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    val outer = BeliefState(Map('A -> Some(true), 'B -> None))
    outer includes inner shouldBe true
  }

  it should "not be included if all predicates have same values" in {
    val inner = BeliefState(Map('A -> Some(true), 'B -> Some(false)))
    val outer = BeliefState(Map('A -> Some(true), 'B -> Some(false), 'C -> Some(true)))
    outer includes inner shouldBe false
    inner includes outer shouldBe true
  }

  it should "include a subset of itself" in {
    val inner = BeliefState(Map('L -> Some(true)))
    val outer = BeliefState(Map('L -> Some(true), 'B -> Some(true), 'S -> Some(true)))
    outer includes inner shouldBe false
    inner includes outer shouldBe true
  }

  it should "treat missing predicates as unknown" in {
    BeliefState('C) includes BeliefState('A.? & ~'B & 'C) shouldBe true
    BeliefState('A & ~'B) includes BeliefState('A) shouldBe false
  }

}
