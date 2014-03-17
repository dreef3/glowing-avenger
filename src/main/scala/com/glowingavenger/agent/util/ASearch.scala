package com.glowingavenger.agent.util

import scala.collection.SortedSet

/**
 * Special version of A* Search algorithm for the case when graph is dynamically generated on the fly.
 */
trait ASearch[N] {

  private class Node(val v: N, val hCost: Int, val parent: Option[Node]) extends Ordered[Node] {
    val totalCost = cost(v) + hCost + (if (parent.isDefined) parent.get.totalCost else 0)

    override def compare(that: Node): Int = totalCost.compare(that.totalCost)
  }

  def successors(node: N): List[N]

  def cost(node: N): Int

  def heruisticCost(init: N, goal: N): Int

  def isGoal(node: N, goal: N): Boolean

  def search(init: N, goal: N): Option[List[N]] = {
    def successorNodes(node: Node) = {
      SortedSet(successors(node.v).map(new Node(_, heruisticCost(node.v, goal), Some(node))): _*)
    }

    def doSearch(frontier: SortedSet[Node], explored: SortedSet[Node], goal: N): Option[List[N]] = {
      if (frontier.isEmpty)
        None
      else {
        val best = frontier.head
        if (isGoal(best.v, goal))
          Some(solution(best))
        else {
          var newFrontier = frontier - best
          val newExplored = explored + best
          for (child <- successorNodes(best)) {
            newFrontier = if (!newExplored.exists(_.v == child.v) && !newFrontier.exists(_.v == child.v)) {
              newFrontier + child
            } else newFrontier.find(n => n.v == child.v && n.totalCost > child.totalCost) match {
              case Some(n) => newFrontier - n + child
              case None => newFrontier
            }
          }
          doSearch(newFrontier, newExplored, goal)
        }
      }
    }

    doSearch(SortedSet(new Node(init, heruisticCost(init, goal), None)), SortedSet[Node](), goal)
  }

  private def solution(node: Node): List[N] = {
    node.parent match {
      case Some(p) => solution(p) ++ List(node.v)
      case None => node.v :: Nil
    }
  }
}
