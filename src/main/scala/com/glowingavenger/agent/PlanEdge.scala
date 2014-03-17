package com.glowingavenger.agent

import scalax.collection.GraphEdge.{NodeProduct, ExtendedKey, EdgeCopy, DiEdge}
import scalax.collection.GraphPredef.OuterEdge

class PlanEdge[N](nodes: Product, val action: Action) extends DiEdge[N](nodes) with EdgeCopy[PlanEdge] with OuterEdge[N, PlanEdge] with ExtendedKey[N] {
  require(!action.name.isEmpty)
  override def keyAttributes: Seq[Any] = Seq(action.name)
  override def copy[NN](nodes: Product) = new PlanEdge[NN](nodes, action)
  override protected def attributesToString = " (" + action + ")"
}

object PlanEdge {
  def apply(init: BeliefState, result: BeliefState, action: Action) = new PlanEdge[BeliefState](NodeProduct(init, result), action)
  def unapply(edge: PlanEdge[BeliefState]) = Some(edge)
}

final class PlaneEdgeAssoc(val edge: DiEdge[BeliefState]) {
  @inline def ## (action: Action) = new PlanEdge[BeliefState](edge.nodes, action)
}
object PlaneEdgeImplicits {
  @inline final implicit def edge2PlaneEdgeAssoc(edge: DiEdge[BeliefState]) = new PlaneEdgeAssoc(edge)
}
