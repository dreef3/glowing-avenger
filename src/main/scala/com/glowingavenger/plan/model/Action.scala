package com.glowingavenger.plan.model

import scala.collection.SortedMap

trait Action {
  val name: String

  def cost: Int

  def attrs: List[Symbol]

  def applicableIn(state: BeliefState): Boolean

  def result(state: BeliefState): BeliefState

  override def toString: String = name
}

case class Question(attr: Symbol) extends Action {
  override def result(state: BeliefState): BeliefState = {
    if (applicableIn(state))
      Answer(state.attrs, attr)
    else state
  }

  override def applicableIn(state: BeliefState): Boolean = {
    state.attrs.contains(attr) && state.attrs(attr) == None
  }

  override def attrs: List[Symbol] = attr :: Nil

  override val name: String = attr.name

  override val cost: Int = 0
}

case class NoAction() extends Action {
  override def result(state: BeliefState): BeliefState = state

  override def applicableIn(state: BeliefState): Boolean = false

  override def attrs: List[Symbol] = Nil

  override val name: String = NoAction.getClass.getSimpleName

  override val cost: Int = 0
}

case class TryAction(override val name: String, options: List[Action]) extends Action {
  private val sortedOptions = SortedMap(options map (a => (a, a.cost)): _*)(Ordering[Int].on(_.cost))

  override def result(state: BeliefState): BeliefState = doTry(state, sortedOptions)

  private def doTry(state: BeliefState, options: SortedMap[Action, Int]): BeliefState = {
    if (options.isEmpty)
      state
    else options.head._1.result(state) match {
      case s if s == state => doTry(state, options.tail)
      case o => o
    }
  }

  override def applicableIn(state: BeliefState): Boolean = options.exists(_.applicableIn(state))

  override def attrs: List[Symbol] = (options.head.attrs /: options.tail)(_ ++ _.attrs).toSet.toList

  override val cost: Int = options.maxBy(_.cost).cost
}
