package com.glowingavenger.plan.impl

import com.glowingavenger.plan.util.ASearch
import scala.Some
import com.glowingavenger.plan.ActionEdge
import com.glowingavenger.plan.model.action.{NoAction, Action, Question}
import com.glowingavenger.plan.model.state.{Answer, BeliefState}

trait Successors {
  protected def successors(state: BeliefState): List[ActionEdge] = Nil
}

trait QuestionSuccessors extends Successors with Axioms {
  override protected def successors(state: BeliefState): List[ActionEdge] = {
    super.successors(state) ::: state.unknown.map(Question).map {
      question =>
        question.result(state) match {
          case a: Answer =>
            ActionEdge(state, applyAxiom(a.yes), question) ::
              ActionEdge(state, applyAxiom(a.no), question) :: Nil
          case _ => Nil
        }
    }.flatten
  }
}

trait GuaranteedPathSuccessors extends Successors with Axioms with ProblemAware {
  val guaranteed = new ASearch[(BeliefState, Action)] {
    override def isGoal(node: (BeliefState, Action), goal: (BeliefState, Action)): Boolean = goal._1 includes node._1

    override def hCost(init: (BeliefState, Action), goal: (BeliefState, Action)): Int = 0

    override def cost(node: (BeliefState, Action)): Int = 1

    override def successors(node: (BeliefState, Action)): List[(BeliefState, Action)] = for {
      action <- problem.domain.actions
      result = action.result(node._1)
      if result != node._1
    } yield (applyAxiom(result), action)
  }

  private def path2Edges(path: List[(BeliefState, Action)]): List[ActionEdge] = {
    if (path.isEmpty || path.tail.isEmpty)
      Nil
    else
      ActionEdge(path.head._1, path.tail.head._1, path.tail.head._2) :: Nil ::: path2Edges(path.tail)
  }

  private def guaranteedPath(state: BeliefState, producer: Action = NoAction()) = {
    guaranteed.search((state, producer), (BeliefState(problem.goal), NoAction())) match {
      case Some(path) => path2Edges(path)
      case None => Nil
    }
  }

  override protected def successors(state: BeliefState): List[ActionEdge] = {
    super.successors(state) ::: guaranteedPath(state)
  }
}
