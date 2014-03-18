package com.glowingavenger.agent.plan

import com.glowingavenger.agent.problem._
import scala.collection.SortedSet
import com.glowingavenger.agent.problem.AskAction
import com.glowingavenger.agent.problem.Answer
import com.glowingavenger.agent.problem.NoAction
import com.glowingavenger.agent.util.Model
import org.sat4j.scala.Logic._

class ContingentPlan(val problem: Problem) {
  val init = Node(problem.init, Nil, problem.heruistic(problem.init, problem.goal))
  val goalWithKB = BeliefState.fromBoolExp(problem.goal.asBoolExp & problem.kb)
  val kbModel = Model.retrieveModelsOrThrow(problem.kb)

  private def applyKB(state: BeliefState): BeliefState = {
    if (isSat(state.asBoolExp & problem.kb)._1)
      state
    else {
      val relaxedAttrs = for {
        (s, v) <- state.attrs
        if v != None
        if init.state.attrs.contains(s) && init.state.attrs(s) != None
      } yield (s, None)
      val relaxed = new BeliefState(state.attrs ++ relaxedAttrs)
      BeliefState.fromBoolExp(relaxed.asBoolExp & problem.kb, state.attrs.keys)
    }
  }

  private def successors(node: Node): List[Edge] = {
    for {
      action <- problem.actions
      intermediate = action.result(node.state)
      if intermediate != node.state
      result = applyKB(intermediate)
    } yield Edge(node, Node(node, result, action, problem), action)
  }

  private def askSuccessors(node: Node): List[Edge] = {
    node.state.unknown.map(AskAction).map {
      ask =>
        ask.result(node.state) match {
          case a: Answer => Edge(node, Node(node, a.yes, ask, problem), ask) :: Edge(node, Node(node, a.no, ask, problem), ask) :: Nil
          case _ => Nil
        }
    }.flatten
  }

  private def bestSuccessor(node: Node): Edge = {
    successors(node).minBy(e => e.end.totalCost)
  }

  def create(): Option[List[Edge]] = {
    val frontier = SortedSet(Edge(init, init, NoAction()))
    val explored = SortedSet[Edge]()
    doSearch(frontier, explored, problem.goal) match {
      case None => None
      case Some(e) => Some(solution(e))
    }
  }

  private def isGoal(state: BeliefState): Boolean = {
    val s = state.asBoolExp
    val sWithKB = BeliefState.fromBoolExp(s & problem.kb)
    goalWithKB includes sWithKB
  }

  private def doSearch(frontier: SortedSet[Edge], explored: SortedSet[Edge], goal: BeliefState): Option[Edge] = {
    if (frontier.isEmpty)
      None
    else {
      val best = frontier.head
      if (isGoal(best.end.state))
        Some(best)
      else {
        var newFrontier = frontier - best
        val newExplored = explored + best
        for (child <- successors(best.end) ::: askSuccessors(best.end)) {
          val node = child.end
          newFrontier = if (!newExplored.exists(_.end.state == node.state) && !newFrontier.exists(_.end.state == node.state)) {
            newFrontier + child
          } else newFrontier.find(edge => edge.end.state == node.state && edge.end.totalCost > node.totalCost) match {
            case Some(e) => newFrontier - e + child
            case None => newFrontier
          }
        }
        doSearch(newFrontier, newExplored, goal)
      }
    }
  }

  private def solution(last: Edge): List[Edge] = {
    last.action match {
      case NoAction() => Nil
      case a => last.start.in match {
        case Some(e) => solution(e) ::: last.start.out
        case None => Nil
      }
    }
  }
}
