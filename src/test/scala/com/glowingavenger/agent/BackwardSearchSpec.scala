package com.glowingavenger.agent

import org.sat4j.scala.Logic._
import com.glowingavenger.agent.problem.LogicAction

class BackwardSearchSpec extends PlanSpec {
  behavior of "Backward search"

  it should "return None when it's not possible to achieve the goal" in {
    val agent = new BackwardSearch(List('L, 'S, 'B), Nil, None)
    val init: BoolExp = ~'L
    val goal: BoolExp = 'L

    val res = agent.search(init, goal)
    res shouldBe None
  }

  it should "return symbols when goal matches init" in {
    val agent = new BackwardSearch(List('L, 'S, 'B), Nil, None)
    val init: BoolExp = 'L
    val goal: BoolExp = 'L

    val res = agent.search(init, goal)
    res shouldBe Some(Map('L -> Some(true)))
  }

  it should "return symbols and execute when there's an action which can achieve the goal" in {
    val init: BoolExp = ~'L
    val goal: BoolExp = 'L

    val agent = new BackwardSearch(List('L, 'S, 'B), LogicAction(init, goal) :: Nil, None)

    val res = agent.search(init, goal)
    res shouldBe Some(Map('L -> Some(false)))
  }

  it should "find and execute a sequence of actions when there's no single action which can achieve the goal" in {
    val init: BoolExp = ~'L
    val goal: BoolExp = 'L

    val agent = new BackwardSearch(List('L, 'S, 'B), List(LogicAction(~'B, goal), LogicAction('B, ~'B)), None)

    val res = agent.search(init, goal)
    res shouldBe Some(Map('L -> Some(false), 'B -> None))
  }

  it should "pass a more complex example" in new ProblemEnv {
    val algo = new BackwardSearch(List('L, 'S, 'B, 'N, 'D), actions, Some('L iff 'S & 'B))

    val res = algo.search(init.asBoolExp, goal.asBoolExp)
    res shouldBe Some(Map('S -> None, 'L -> Some(false), 'B -> None))
  }
}
