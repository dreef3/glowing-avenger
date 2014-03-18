package com.glowingavenger.agent.plan

import com.glowingavenger.agent.problem._

class Edge(startNode: Node, endNode: Node, val action: Action) extends Ordered[Edge] {
  val start = Node(startNode, this)
  val end = Node.produce(endNode, this)

  override def compare(that: Edge): Int = end.compare(that.end)
}

object Edge {
  def apply(start: Node, end: Node, action: Action) = new Edge(start, end, action)
}

case class Node(state: BeliefState, out: List[Edge], totalCost: Int, in: Option[Edge] = None) extends Ordered[Node] {
  override def compare(that: Node): Int = totalCost.compare(that.totalCost)
}

object Node {
  def apply(existing: Node, out: Edge): Node = {
    Node(existing.state, existing.out ::: out :: Nil, existing.totalCost, existing.in)
  }

  def produce(n: Node, p: Edge): Node = Node(n.state, n.out, n.totalCost, Some(p))

  def apply(parent: Node, state: BeliefState, producer: Action, problem: Problem): Node = {
    Node(state, Nil, parent.totalCost + producer.cost + problem.heruistic(parent.state, problem.goal))
  }
}

