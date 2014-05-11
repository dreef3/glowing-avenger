package com.glowingavenger.plan

import com.glowingavenger.plan.impl._
import com.glowingavenger.plan.model.Problem

class ContingencyPlan(override val problem: Problem) extends AbstractPlanner
with BackwardSearchInit with DefaultAxioms with GuaranteedPathSuccessors with QuestionSuccessors {

}

object ContingencyPlan {
  def apply(problem: Problem) = new ContingencyPlan(problem).build()
}

/*
package com.glowingavenger.plan

import com.glowingavenger.plan.model._
import scala.collection.immutable.Queue
import com.glowingavenger.plan.util.{ReachGraph, ASearch}
import org.jgrapht.DirectedGraph
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import ReachGraph.graphToReachGraph
import org.sat4j.scala.Logic.True
import com.glowingavenger.plan.model.state.Answer
import scala.Some
import com.glowingavenger.plan.model.action.{NoAction, LogicAction, Action, Question}
import com.glowingavenger.plan.impl.BackwardSearch

object ContingencyPlan {
  def apply(problem: Problem) = new ContingencyPlan(problem)

  def build(problem: Problem) = ContingencyPlan(problem).build()
}





class ContingencyPlan(val problem: Problem) {
  val guaranteed = new ASearch[(BeliefState, Action)] {
    override def isGoal(node: (BeliefState, Action), goal: (BeliefState, Action)): Boolean = goal._1 includes node._1

    override def hCost(init: (BeliefState, Action), goal: (BeliefState, Action)): Int = 0

    override def cost(node: (BeliefState, Action)): Int = 1

    override def successors(node: (BeliefState, Action)): List[(BeliefState, Action)] = for {
      action <- problem.domain.actions
      result = action.result(node._1)
      if result != node._1
    } yield (applyAxioms(result), action)
  }

  // TODO This is a naive approach that would work only with a single axiom
  val axiom = if (problem.domain.axioms.isEmpty) True
  else (problem.domain.axioms.head /: problem.domain.axioms.tail)(_ & _)

  val backward = new BackwardSearch(problem.domain.predicates,
    for (a <- problem.domain.actions if a.isInstanceOf[LogicAction]) yield a.asInstanceOf[LogicAction],
    Some(axiom))

  def build(): PlanDescription = {
    val init = backward.search(problem.init, problem.goal) match {
      case Some(c) => BeliefState(c)
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
      val answers = questionSuccessors(next)
      val additions = answers.unzip._1.toIterable
      val guaranteedPath = pathToGoal(next)
      buildRec(nextQueue ++ additions, plan ++ (guaranteedPath ::: list2Edges(next, answers)))
    }
  }

  private def pathToGoal(state: BeliefState, producer: Action = NoAction()) = {
    guaranteed.search((state, producer), (BeliefState(problem.goal), NoAction())) match {
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

  private def applyAxioms(state: BeliefState): BeliefState = {
    BeliefState(state.asBoolExp & axiom, state.attrs.keys)
  }

  private def questionSuccessors(state: BeliefState): List[(BeliefState, Action)] = {
    state.unknown.map(Question).map {
      question =>
        question.result(state) match {
          case a: Answer => (applyAxioms(a.yes), question) ::(applyAxioms(a.no), question) :: Nil
          case _ => Nil
        }
    }.flatten
  }
}





 */
