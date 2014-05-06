package com.glowingavenger.plan.model

import org.sat4j.scala.Logic._
import com.glowingavenger.plan.util.Model

object BeliefState {
  def fromBoolExp(clause: BoolExp, attrs: Iterable[Symbol] = Nil): BeliefState = {
    val unknowns = attrs.map(attr => (attr, None.asInstanceOf[Option[Boolean]])).toMap
    Model.retrieveModels(clause) match {
      case Some(clauseAttrs) => new BeliefState(unknowns ++ clauseAttrs)
      case None => throw new IllegalArgumentException("Unsatisfiable clause: " + PrettyPrint(clause))
    }
  }
}

object BeliefStateImplicits {
  @inline implicit def boolExp2BeliefState(exp: BoolExp): BeliefState = BeliefState.fromBoolExp(exp)
}

class BeliefState(val attrs: Map[Symbol, Option[Boolean]]) {
  val unknown = attrs.filter(!_._2.isDefined).keys.toList
  val known = attrs.filter(_._2.isDefined)

  def asBoolExp: BoolExp = {
    val expList = for {
      attr <- attrs
      if attr._2.isDefined
    } yield if (attr._2.get) toProp(attr._1) else ~attr._1
    (expList.head /: expList.tail)(_ & _)
  }

  def includes(state: BeliefState): Boolean = {
    !state.attrs.exists(a => {
      val (s, v) = a
      attrs.contains(s) && attrs(s) != v && attrs(s).isDefined
    })
  }

  def countEqual(state: BeliefState) = {
    attrs.count(p => state.attrs.contains(p._1) && state.attrs(p._1) == p._2)
  }

  override def toString: String = {
    val strAttrs = attrs map {
      case (s, Some(v)) if v => s.name
      case (s, Some(v)) => "-" + s.name
      case (s, None) => s.name + "?"
    }
    strAttrs.mkString("(", " ", ")")
  }

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
