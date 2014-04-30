package com.glowingavenger.plan

import com.glowingavenger.plan.problem._
import scala.collection.immutable.Queue
import com.glowingavenger.plan.util.ASearch
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import scala.collection.JavaConversions._
import ReachGraph.graphToReachGraph

object ContingencyPlan {
  def apply(problem: PDDLProblem) = new ContingencyPlan(problem)

  def build(problem: PDDLProblem) = ContingencyPlan(problem).build()
}

case class ActionEdge(from: BeliefState, to: BeliefState, action: Action) extends DefaultEdge

case class PlanDescription(init: BeliefState, plan: DirectedGraph[BeliefState, ActionEdge], problem: PDDLProblem)

class ContingencyPlan(val problem: PDDLProblem) {
  val guaranteed = new ASearch[(BeliefState, Action)] {
    override def isGoal(node: (BeliefState, Action), goal: (BeliefState, Action)): Boolean = goal._1 includes node._1

    override def hCost(init: (BeliefState, Action), goal: (BeliefState, Action)): Int = 0

    override def cost(node: (BeliefState, Action)): Int = 1

    override def successors(node: (BeliefState, Action)): List[(BeliefState, Action)] = for {
      action <- problem.actions
      result = action.result(node._1)
      if result != node._1
    } yield (withKB(result), action)
  }

  val backward = new BackwardSearch(problem.attrs,
    for (a <- problem.actions if a.isInstanceOf[LogicAction]) yield a.asInstanceOf[LogicAction],
    Some(problem.kb))

  def build(): PlanDescription = {
    val init = backward.search(problem.init.asBoolExp, problem.goal.asBoolExp) match {
      case Some(c) => new BeliefState(c)
      case None => throw new UnsupportedOperationException("Goal is unreachable")
    }
    val front = Queue(init)
    val empty = new DefaultDirectedGraph[BeliefState, ActionEdge](classOf[ActionEdge])
    val graph = buildRec(front, empty)
    PlanDescription(init, graph, problem)
  }

  private def buildRec(front: Queue[BeliefState], plan: DirectedGraph[BeliefState, ActionEdge]): DirectedGraph[BeliefState, ActionEdge] = {
    if (front.isEmpty) plan
    else {
      val (next, nextQueue) = front.dequeue
      val askers = askSuccessors(next)
      val additions = askers.unzip._1.toIterable
      val guaranteedPath = pathToGoal(next)
      buildRec(nextQueue ++ additions, plan ++ (guaranteedPath ::: list2Edges(next, askers)))
    }
  }

  private def pathToGoal(state: BeliefState, producer: Action = NoAction()) = {
    guaranteed.search((state, producer), (problem.goal, NoAction())) match {
      case Some(path) => path2Edges(path)
      case None => Nil
    }
  }

  private def list2Edges(from: BeliefState, successors: List[(BeliefState, Action)]): List[ActionEdge] = {
    for {
      (to, action) <- successors
    } yield ActionEdge(from, to, action)
  }

  private def path2Edges(path: List[(BeliefState, Action)]): List[ActionEdge] = {
    if (path.isEmpty || path.tail.isEmpty)
      Nil
    else
      ActionEdge(path.head._1, path.tail.head._1, path.tail.head._2) :: Nil ::: path2Edges(path.tail)
  }

  private def withKB(state: BeliefState): BeliefState = {
    BeliefState.fromBoolExp(state.asBoolExp & problem.kb, state.attrs.keys)
  }

  private def askSuccessors(state: BeliefState): List[(BeliefState, Action)] = {
    state.unknown.map(Question).map {
      ask =>
        ask.result(state) match {
          case a: Answer => (withKB(a.yes), ask) ::(withKB(a.no), ask) :: Nil
          case _ => Nil
        }
    }.flatten
  }
}

class ReachGraph(g: DirectedGraph[BeliefState, ActionEdge]) {
  def +++(other: DirectedGraph[BeliefState, ActionEdge]): DirectedGraph[BeliefState, ActionEdge] = {
    val newGraph = new DefaultDirectedGraph[BeliefState, ActionEdge](classOf[ActionEdge])
    for (v <- g.vertexSet()) newGraph.addVertex(v)
    for (v <- other.vertexSet()) newGraph.addVertex(v)
    for (e <- g.edgeSet()) newGraph.addEdge(e.from, e.to, e)
    for (e <- other.edgeSet()) newGraph.addEdge(e.from, e.to, e)
    newGraph
  }

  def ++(edges: Iterable[ActionEdge]): DirectedGraph[BeliefState, ActionEdge] = {
    val newGraph = g +++ new DefaultDirectedGraph[BeliefState, ActionEdge](classOf[ActionEdge])
    for (e <- edges) {
      newGraph.addVertex(e.from)
      newGraph.addVertex(e.to)
      newGraph.addEdge(e.from, e.to, e)
    }
    newGraph
  }

  def +(edge: ActionEdge): DirectedGraph[BeliefState, ActionEdge] = {
    val newGraph = g +++ new DefaultDirectedGraph[BeliefState, ActionEdge](classOf[ActionEdge])
    newGraph.addVertex(edge.from)
    newGraph.addVertex(edge.to)
    newGraph.addEdge(edge.from, edge.to, edge)
    newGraph
  }
}

object ReachGraph {
  @inline implicit def graphToReachGraph(g: DirectedGraph[BeliefState, ActionEdge]): ReachGraph = new ReachGraph(g)
}
