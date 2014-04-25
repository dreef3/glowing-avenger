package com.glowingavenger.plan.util

import org.sat4j.scala.Logic._
import Model._
import org.scalatest.{FlatSpec, Matchers}

class ModelSpec extends FlatSpec with Matchers {
  behavior of "Model utility"

  it should "throw if the clause is unsatisfiable" in {
    a [IllegalArgumentException] should be thrownBy {
      retrieveModelsOrThrow('A & ~'A)
    }
  }

  it should "produce a model containing all symbols from clause" in {
    retrieveModelsOrThrow(('A | ~'A) & 'B) shouldBe Map('A -> None, 'B -> Some(true))
  }

  it should "produce a model with all symbols undefined for trivial clauses" in {
    retrieveModelsOrThrow('A | ~'A) shouldBe Map('A -> None)
  }

  it should "unset symbols in model it they weren't produced by the clause" in {
    retrieveSymbols(Map('A -> Some(true), 'B -> Some(true)), ('A | ~'A) & 'B) shouldBe Some(Map('A -> None, 'B -> Some(true)))
  }
}
