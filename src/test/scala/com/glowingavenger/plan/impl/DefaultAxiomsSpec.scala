package com.glowingavenger.plan.impl

import com.glowingavenger.plan.PlanSpec

class DefaultAxiomsSpec extends PlanSpec {
  trait TestEnv extends DefaultAxioms with MixedPathSuccessors with ProblemEnv {
  }
}
