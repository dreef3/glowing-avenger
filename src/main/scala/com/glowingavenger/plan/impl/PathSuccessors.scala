package com.glowingavenger.plan.impl

import com.glowingavenger.plan.model.state.{Answer, BeliefState}
import com.glowingavenger.plan.ActionEdge
import com.glowingavenger.plan.util.ASearch
import com.glowingavenger.plan.model.action.{Question, NoAction, Action}
import org.sat4j.scala.Logic.BoolExp
import com.glowingavenger.plan.model.Problem

trait PathSuccessors extends Successors with Axioms with ProblemAware {
  protected val search = new ASearch[(BeliefState, Action)] {
    override def isGoal(node: (BeliefState, Action), goal: (BeliefState, Action)): Boolean = isSearchGoal(node, goal)

    override def hCost(init: (BeliefState, Action), goal: (BeliefState, Action)): Int = 0

    override def cost(node: (BeliefState, Action)): Int = 1

    override def successors(node: (BeliefState, Action)): List[(BeliefState, Action)] = searchSuccessors(node)
  }

  protected def searchSuccessors(node: (BeliefState, Action)): List[(BeliefState, Action)]

  protected def isSearchGoal(node: (BeliefState, Action), goal: (BeliefState, Action)): Boolean =
    goal._1 includes node._1

  protected def path2Edges(path: List[(BeliefState, Action)]): List[ActionEdge] = {
    if (path.isEmpty || path.tail.isEmpty)
      Nil
    else
      ActionEdge(path.head._1, path.tail.head._1, path.tail.head._2) :: Nil ::: path2Edges(path.tail)
  }

  protected def makePath(state: BeliefState, producer: Action = NoAction()) = {
    search.search((state, producer), (BeliefState(problem.goal), NoAction())) match {
      case Some(path) => path2Edges(path)
      case None => Nil
    }
  }

  protected def pathStates(path: List[ActionEdge]): List[BeliefState] = Nil

  override def successors(state: BeliefState): (List[ActionEdge], List[BeliefState]) = {
    val (edges, states) = super.successors(state)
    val path = makePath(state)
    (edges ::: path, states ::: pathStates(path))
  }
}

trait MixedPathSuccessors extends GuaranteedPathSuccessors {
  private val questionSuccessors = new QuestionSuccessors {
    override protected def applyAxiom(clauses: BeliefState*): BeliefState = MixedPathSuccessors.this.applyAxiom(clauses: _*)

    override protected def axiom(axioms: Iterable[BoolExp]): (Iterable[BeliefState]) => BoolExp =
      MixedPathSuccessors.this.axiom(axioms)

    def searchSuccessors(node: (BeliefState, Action)): List[(BeliefState, Action)] = {
      successors(node._1)._1 map {
        case ActionEdge(f, t, a) => (t, a)
      }
    }
  }

  override protected def searchSuccessors(node: (BeliefState, Action)): List[(BeliefState, Action)] = {
    super.searchSuccessors(node) ::: questionSuccessors.searchSuccessors(node)
  }

  override protected def makePath(state: BeliefState, producer: Action = NoAction()) = {
    val bestPath = super.makePath(state, producer)
    val answerBranches = bestPath collect {
      case ActionEdge(f, t, q: Question) =>
        q.result(f) match {
          case a: Answer if a.yes includes t => ActionEdge(f, applyAxiom(a.no), q)
          case a: Answer => ActionEdge(f, applyAxiom(a.yes), q)
        }
    }

    bestPath ::: answerBranches
  }

  override protected def pathStates(path: List[ActionEdge]): List[BeliefState] = path collect {
    // Need to process vertices with more than one unknown attribute
    case ActionEdge(_, to, _) if !to.unknown.isEmpty => to
  }
}

trait GuaranteedPathSuccessors extends PathSuccessors {
  protected def searchSuccessors(node: (BeliefState, Action)): List[(BeliefState, Action)] = {
    for {
      action <- problem.domain.actions
      result = action.result(node._1)
      if result != node._1
    } yield (applyAxiom(result), action)
  }
}

/**
 * MixedPathSuccessors and GuaranteedPathSuccessors cannot be stacked together because
 * the first one inherits from another and overrides it's methods
 */
trait CompoundPathSuccessors extends Successors with Axioms with ProblemAware {
  val mixedPath = new MixedPathSuccessors {
    override protected def applyAxiom(clauses: BeliefState*): BeliefState =
      CompoundPathSuccessors.this.applyAxiom(clauses: _*)

    override protected def axiom(axioms: Iterable[BoolExp]): (Iterable[BeliefState]) => BoolExp =
      CompoundPathSuccessors.this.axiom(axioms)

    override def problem: Problem = CompoundPathSuccessors.this.problem
  }

  val guaranteedPath = new GuaranteedPathSuccessors {
    override protected def applyAxiom(clauses: BeliefState*): BeliefState =
      CompoundPathSuccessors.this.applyAxiom(clauses: _*)

    override protected def axiom(axioms: Iterable[BoolExp]): (Iterable[BeliefState]) => BoolExp =
      CompoundPathSuccessors.this.axiom(axioms)

    override def problem: Problem = CompoundPathSuccessors.this.problem
  }

  override def successors(state: BeliefState): (List[ActionEdge], List[BeliefState]) = {
    val (edges, states) = super.successors(state)
    val guaranteed = guaranteedPath.successors(state)
    val mixed = mixedPath.successors(state)
    (edges ::: guaranteed._1 ::: mixed._1, states ::: guaranteed._2 ::: mixed._2)
  }
}
