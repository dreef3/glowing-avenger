package com.glowingavenger.agent

import org.scalatest.{Matchers, FlatSpec}
import Prop._
import org.sat4j.scala.Logic._

class SuperAgentSpec extends FlatSpec with Matchers {
 trait Env {
   val props = unknown("L") :: unknown("B") :: unknown("S") :: Nil
   val (on, off, ch) = (Action("on", ~'S, 'S), Action("off", 'S, ~'S), Action("ch", ~'B, 'B))
   val actions = on :: off :: ch :: Nil
   val env = new Environment(actions, props, 'L iff 'B & 'S)
   val goal: BoolExp = 'L
   val init = ~'L
 }

  "Agent" should "find granted way to achieve the goal" in new Env {
    val agent = new Agent(env, init, goal)
    val granted = agent.findGrantedSequence()
    granted shouldEqual off :: ch :: on :: Nil
  }
}
