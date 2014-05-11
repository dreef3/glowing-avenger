package com.glowingavenger.plan.model.state

import org.sat4j.scala.Logic.BoolExp

/**
 * We'd like to use this class to enable use of belief states with generic predicate type. Sat4j already
 * allows us to use whatever we want, not only Symbol.
 * @tparam E generic predicate description type
 */
abstract class AbstractBeliefState[E](val predicates: Map[E, Option[Boolean]]) {
  require(!predicates.isEmpty)

  def unknown: Iterable[Symbol]

  def known: Map[Symbol, Option[Boolean]]

  def toExpr: BoolExp

  /**
   * Determine if one state includes another. The rules are:
   * 1. Missing predicate 'A is treated as 'A -> None (i.e., unknown)
   * 2. Unknown predicate 'A -> None includes both 'A -> Some(true), 'A -> Some(false)
   * 3. Predicate with known value includes only itself
   *
   * @param other state to test for inclusion in current
   * @return true if other state is included in this by described rules
   */
  def includes(other: AbstractBeliefState[E]): Boolean = {
    !other.predicates.exists {
      case (s, v) => predicates.contains(s) && predicates(s) != v && predicates(s).isDefined
    } && !predicates.exists {
      case (s, v) =>
        !other.predicates.contains(s) && v.isDefined
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[AbstractBeliefState[E]]

  override def equals(other: Any): Boolean = other match {
    case that: AbstractBeliefState[E] =>
      (that canEqual this) &&
        predicates == that.predicates
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(predicates)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
