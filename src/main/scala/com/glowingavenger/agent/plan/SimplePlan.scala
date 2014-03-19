package com.glowingavenger.agent.plan

import com.glowingavenger.agent.problem._
import scalax.collection.Graph
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LBase._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.WLDiEdge
import scala.collection.immutable.Queue

class SimplePlan(val problem: Problem) {
  def build(): Graph[BeliefState, WLDiEdge] = {
    val front = Queue(problem.init)
    val start = Graph[BeliefState, WLDiEdge]()
    Graph[BeliefState, WLDiEdge]()
  }

  def successors(state: BeliefState) = {
    for {
      action <- problem.actions
      result = action.result(state)
      if result != state
    } yield result
  }
}