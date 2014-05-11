package com.glowingavenger.plan.model.state

import com.glowingavenger.plan.util.Model
import org.sat4j.scala.Logic._

object BeliefState {
  def apply(clause: BoolExp, attrs: Iterable[Symbol] = Nil): BeliefState = new BeliefState(clause, attrs)

  def apply(attrs: Map[Symbol, Option[Boolean]]): BeliefState = new BeliefState(attrs, None)

  def unapply(state: BeliefState): Option[BoolExp] = Some(state.toExpr)
}

class BeliefState protected(attrs: Map[Symbol, Option[Boolean]], expr: Option[BoolExp] = None)
  extends AbstractBeliefState[Symbol](attrs) {

  override lazy val unknown = predicates.filter(!_._2.isDefined).keys.toList
  override lazy val known = predicates.filter(_._2.isDefined)

  private lazy val boolExp = expr match {
    case Some(e) => e
    case _ =>
      val expList = predicates collect {
        case (s, Some(v)) if v => toProp(s)
        case (s, Some(v)) if !v => ~s
        case (s, _) => toProp(s).?
      }
      (expList.head /: expList.tail)(_ & _)
  }

  protected def this(clause: BoolExp, attrs: Iterable[Symbol] = Nil) = {
    this(attrs.map(attr => (attr, None.asInstanceOf[Option[Boolean]])).toMap ++
      Model.retrieveModelsOrThrow(clause), Some(clause))
  }

  override def toExpr: BoolExp = boolExp

  def countEqual(state: BeliefState) = {
    predicates.count(p => state.predicates.contains(p._1) && state.predicates(p._1) == p._2)
  }

  override def toString: String = {
    val strAttrs = predicates map {
      case (s, Some(v)) if v => s.name
      case (s, Some(v)) => "-" + s.name
      case (s, None) => s.name + "?"
    }
    strAttrs.mkString("(", " ", ")")
  }
}

object BeliefStateImplicits {
  @inline implicit def boolExp2BeliefState(exp: BoolExp): BeliefState = BeliefState.apply(exp)
}
