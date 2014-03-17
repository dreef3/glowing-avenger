package com.glowingavenger.agent

import org.sat4j.scala.Logic._
import scalax.collection.Graph
import scalax.collection.GraphPredef._,
scalax.collection.GraphEdge._
import language.{higherKinds, postfixOps}
import scalax.collection.{Graph, GraphLike}
import scalax.collection.GraphPredef._,
scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCompanion
import PlaneEdgeImplicits._

object Plan {
  def build(problem: Problem): Plan = {

    def successors(state: BeliefState, actions: List[Action] = problem.actions): List[PlanEdge[BeliefState]] = {
      val pairs = for {
        action <- actions
        result = action.result(state)
        if result != state
      } yield (result, action)
      // TODO Implicit conversion returns type List[Any] instead of List[PlanEdge]
      pairs map (p => PlanEdge(state, p._1, p._2))
    }

    def search(state: BeliefState, partial: Graph[BeliefState, PlanEdge[BeliefState]]): Option[Graph[BeliefState, PlanEdge[BeliefState]]] = {
      if (state includes problem.goal)
        Some(partial)
      else {
        partial.
      }
    }
    null
  }

}

class Plan(val problem: Problem, val steps: List[Action])