package com.glowingavenger.plan

import com.glowingavenger.plan.problem._
import scala.collection.immutable.Queue
import com.glowingavenger.plan.util.ASearch
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.Graphs._
import scala.collection.JavaConversions._

object ContingencyPlan {
  def apply(problem: Problem) = new ContingencyPlan(problem)

  def build(problem: Problem) = ContingencyPlan(problem).build()
}

case class ActionEdge(from: BeliefState, to: BeliefState, action: Action) extends DefaultEdge

case class PlanDescription(init: BeliefState, plan: DirectedGraph[BeliefState, ActionEdge], problem: Problem)

class ContingencyPlan(val problem: Problem) {
  val guaranteed = new ASearch[(BeliefState, Action)]{
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
      // TODO Create GraphImplicits for this
      val newGraph = new DefaultDirectedGraph[BeliefState, ActionEdge](classOf[ActionEdge])
      addGraph(newGraph, plan)
      val edges = guaranteedPath ::: list2Edges(next, askers)
      newGraph.edgeSet().addAll(edges)
      buildRec(nextQueue ++ additions, newGraph)
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
          case a: Answer => (withKB(a.yes), ask) :: (withKB(a.no), ask) :: Nil
          case _ => Nil
        }
    }.flatten
  }
}
