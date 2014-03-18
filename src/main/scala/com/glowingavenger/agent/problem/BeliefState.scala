package com.glowingavenger.agent.problem

import org.sat4j.scala.Logic._
import com.glowingavenger.agent.util.Model

object BeliefState {
  def fromBoolExp(clause: BoolExp, attrs: Iterable[Symbol] = Nil): BeliefState = {
    val unknowns = attrs.map(attr => (attr, None.asInstanceOf[Option[Boolean]])).toMap
    Model.retrieveModels(clause) match {
      case Some(clauseAttrs) => new BeliefState(unknowns ++ clauseAttrs)
      case None => throw new IllegalArgumentException("Unsatisfiable clause: " + PrettyPrint(clause))
    }
  }
}

class BeliefState(val attrs: Map[Symbol, Option[Boolean]]) {
  val unknown = attrs.filter(!_._2.isDefined).keys.toList

  def asBoolExp: BoolExp = {
    val expList = for {
      attr <- attrs
      if attr._2.isDefined
    } yield if (attr._2.get) toProp(attr._1) else ~attr._1
    (expList.head /: expList.tail)(_ & _)
  }

  def includes(state: BeliefState): Boolean = {
    state.attrs.find(e =>
      !attrs.contains(e._1) || (attrs(e._1) != e._2 && attrs(e._1) != None)
    ) == None
  }

  def countEqual(state: BeliefState) = {
    attrs.count(p => state.attrs.contains(p._1) && state.attrs(p._1) == p._2)
  }

  override def toString: String = attrs.mkString(", ")

  def canEqual(other: Any): Boolean = other.isInstanceOf[BeliefState]

  override def equals(other: Any): Boolean = other match {
    case that: BeliefState =>
      (that canEqual this) &&
        attrs == that.attrs
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(attrs)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

case class Answer(override val attrs: Map[Symbol, Option[Boolean]], attr: Symbol) extends BeliefState(attrs) {
  require(attrs.keySet.contains(attr))
  require(attrs(attr) == None)

  val yes = new BeliefState(attrs ++ Map(attr -> Some(true)))
  val no = new BeliefState(attrs ++ Map(attr -> Some(false)))
}

case class ProducedState(override val attrs: Map[Symbol, Option[Boolean]], producer: Action) extends BeliefState(attrs)

object ProducedState {
  @inline implicit def beliefState2ProducedState(state: BeliefState): ProducedState = {
    new ProducedState(state.attrs, NoAction())
  }
}
