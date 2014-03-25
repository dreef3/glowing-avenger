package com.glowingavenger.agent.util

import scalax.collection.Graph
import scalax.collection.edge.WLDiEdge
import com.glowingavenger.agent.problem.{Action, BeliefState}
import scalax.collection.io.dot._
import scala.sys.process._

object DotExport {
  private val DotAttrLabel = "label"
  private val DefaultFormat = "png"

  /**
   * Produces a contingent plan graph image.
   *
   * @param args First arg is input file name. Second arg is output file name. Third (optional) is format, PNG by default.
   */
  def main (args: Array[String]) {
    require(args.size > 1 && args.size <= 3)
    val input = args(0)
    val output = args(1)
    val format = if (args.size > 2) args(2) else DefaultFormat
    val graphvizCmd = s"dot -T$format -O$output $input"
    graphvizCmd.!
  }

  def export(plan: Graph[BeliefState, WLDiEdge]) = {
    val root = DotRootGraph(directed = true, id = Some("Contingent_Plan"))

    def edgeTransformer(innerEdge: Graph[BeliefState, WLDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
      val edge = innerEdge.edge
      val label = edge.label.asInstanceOf[Action]
      Some(root, DotEdgeStmt(edge.from.toString(), edge.to.toString(), List(DotAttr(DotAttrLabel, label.toString))))
    }

    graph2DotExport(plan).toDot(dotRoot = root, edgeTransformer = edgeTransformer)
  }
}
