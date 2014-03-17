package com.glowingavenger.agent

trait Action {
  val name: String
  def attrs: List[Symbol]
  def applicableIn(state: BeliefState): Boolean
  def result(state: BeliefState): BeliefState
}