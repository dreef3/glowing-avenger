package com.glowingavenger.dialog

import com.glowingavenger.plan.model.{PlanProblem, Question, Action, BeliefState}
import com.glowingavenger.plan.ActionEdge

import scala.swing._
import Dialog.Options._
import Dialog.Result._
import com.glowingavenger.agent.PlanExecutor
import com.glowingavenger.plan.importexport.PDDL

class SimpleSwingDialogAgent extends AbstractDialogAgent with ChooseFirstQuestion with ChooseFirstAction {
  override def onFinish(state: BeliefState, producer: Option[ActionEdge]) {
    Dialog.showMessage(message = "Hooray!")
  }

  override def onFailure(state: BeliefState, producer: Option[ActionEdge]) {
    Dialog.showMessage(message = "Ooops... Something went wrong!")
  }

  protected override def handleAction(action: Action): Boolean = {
    Dialog.showConfirmation(message = s"Please do the following: ${action.name}",
      optionType = OkCancel) match {
      case Ok => true
      case _ => false
    }
  }

  protected override def handleQuestion(question: Question): Option[Boolean] = {
    Dialog.showConfirmation(message = s"Is it true? ${question.name}\nPress [Cancel] if you don't know",
    optionType = YesNoCancel) match {
      case Yes => Some(true)
      case No => Some(false)
      case _ => None
    }
  }
}

object SwingExample {
  def main(args: Array[String]) {
    new PlanExecutor(PDDL.importProblem("src/main/resources/Bulb.pddl"), new SimpleSwingDialogAgent).exec()
  }
}
