package com.glowingavenger.dialog

import com.glowingavenger.agent.PlanTraversalListener
import com.glowingavenger.plan.ActionEdge
import scala.collection.SortedSet
import com.glowingavenger.plan.model.action.{LogicAction, Action, Question}
import com.glowingavenger.plan.model.state.BeliefState

abstract class AbstractDialogAgent extends PlanTraversalListener {
  override def onStateChange(before: Option[BeliefState], state: BeliefState,
                             producer: Option[ActionEdge], successors: Iterable[ActionEdge]): ActionEdge = {
    val (questions, actions) = QuestionsActions(successors).separate

    if (!questions.isEmpty) {
      askQuestions(questions)
    } else {
      actions find (p => handleAction(p._1)) match {
        case Some((a, e)) => e
        case None => throw new FailedPerformActionException()
      }
    }
  }

  private def askQuestions(questions: Map[Question, (ActionEdge, ActionEdge)]): ActionEdge = {
    if (questions.isEmpty) {
      throw new FailedToAskQuestionException()
    }
    val q = chooseQuestion(questions)
    handleQuestion(q) match {
      case Some(b) => if (b) questions(q)._1 else questions(q)._2
      case None => askQuestions(questions - q)
    }
  }

  protected def chooseQuestion(questions: Map[Question, (ActionEdge, ActionEdge)]): Question

  protected def chooseAction(actions: Map[Action, ActionEdge]): Action

  protected def handleAction(action: Action): Boolean

  protected def handleQuestion(question: Question): Option[Boolean]
}

trait ChooseBestAction {
  protected def chooseAction(actions: Map[Action, ActionEdge]): Action =
    SortedSet(actions.keys.toSeq :_*)(Ordering.by[Action, Int](_.cost)).head
}

trait ChooseFirstAction {
  protected def chooseAction(actions: Map[Action, ActionEdge]): Action = actions.keySet.head
}

trait ChooseFirstQuestion {
  protected def chooseQuestion(questions: Map[Question, (ActionEdge, ActionEdge)]): Question =
    questions.keySet.head
}

class FailedToAskQuestionException(message: String = null) extends Exception(message)

class FailedPerformActionException(message: String = null) extends Exception(message)

case class QuestionsActions(edges: Iterable[ActionEdge]) {
  val questions = (edges filter {
    case ActionEdge(_, _, action: Question) => true
    case _ => false
  } map (e => (e.action.asInstanceOf[Question], e))) groupBy (_._1) map (p => (p._1, (p._2.head._2, p._2.tail.head._2)))

  val actions = (edges filter {
    case ActionEdge(_, _, action: LogicAction) => true
    case _ => false
  } map (e => (e.action, e))).toMap

  val separate = (questions, actions)
}
