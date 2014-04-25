package com.glowingavenger.plan.util

import scala.collection.mutable.Set

/**
 * Special version of A* Search algorithm for the case when graph is dynamically generated on the fly.
 */
trait ASearch[N] {

  private class Node(val v: N, hCost: Int, val parent: Option[Node]) extends Ordered[Node] {
    val totalCost: Int = cost(v) + hCost + (if (parent.isDefined) parent.get.totalCost else 0)

    override def toString: String = v.toString + ", (" + totalCost + ")"

    override def compare(that: Node): Int = totalCost.compare(that.totalCost)
  }

  def successors(node: N): List[N]

  def cost(node: N): Int

  def hCost(init: N, goal: N): Int

  def isGoal(node: N, goal: N): Boolean

  def search(init: N, goal: N): Option[List[N]] = {
    def successorNodes(node: Node) = {
      Set(successors(node.v).map(new Node(_, hCost(node.v, goal), Some(node))): _*)
    }

    def doSearch(frontier: Set[Node], explored: Set[Node], goal: N): Option[List[N]] = {
      if (frontier.isEmpty)
        None
      else {
        val best = frontier.minBy(_.totalCost)
        if (isGoal(best.v, goal))
          Some(solution(best))
        else {
          val newExplored = explored + best
          val it = successors(best.v).toIterator
          var newFrontier = frontier.clone() - best
          while(it.hasNext) {
            val child = it.next()
            if (!explored.exists(_.v == child) && !frontier.exists(_.v == child)) {
              newFrontier.add(new Node(child, hCost(child, goal), Some(best)))
            } else {
              frontier.find(n => n.v == child && n.totalCost > best.totalCost + cost(child)) match {
                case None => ()
                case Some(n) => newFrontier = newFrontier - n + new Node(child, hCost(child, goal), Some(best))
              }
            }
          }

          doSearch(newFrontier, newExplored, goal)
        }
      }
    }

    doSearch(Set(new Node(init, hCost(init, goal), None)), Set[Node](), goal)
  }

  private def solution(node: Node): List[N] = {
    node.parent match {
      case Some(p) => solution(p) ++ List(node.v)
      case None => node.v :: Nil
    }
  }
}
