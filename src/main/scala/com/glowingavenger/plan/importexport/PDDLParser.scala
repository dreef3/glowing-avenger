package com.glowingavenger.plan.importexport

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import org.sat4j.scala.Logic.BoolExp
import org.sat4j.scala.Logic.identFromSymbol
import com.glowingavenger.plan.model.{Problem, UnboundProblem, Domain}
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input
import input.CharArrayReader.EofCh
import scala.collection.mutable.ArrayBuffer
import com.glowingavenger.plan.model.action.LogicAction

object PDDL {
  import PDDLParser._
  private def parse[T](input: String)(implicit p: Parser[T]): T = {
    val parser = phrase(p)
    parser(new PackratReader[PDDLParser.Elem](new lexical.Scanner(input))) match {
      case Success(t: T, _) => t
      case NoSuccess(msg, i) =>
        val a = i
        throw new IllegalArgumentException(msg)
    }
  }

  def importProblem(input: String) = parse(input)(pddl)

  def importDomain(input: String) = parse(input)(domain)
}

private[importexport] object PDDLParser extends StandardTokenParsers with PackratParsers {

  override val lexical = new PDDLLexical

  lazy val domain = ("(" ~> "define" ~> "(" ~> "domain" ~> ident <~ ")") ~ predicatesDef ~ rep(structureDef) <~ ")" ^^ {
    case name ~ predicates ~ structures =>
      val actions = new ArrayBuffer[LogicAction]()
      val axioms = new ArrayBuffer[BoolExp]()
      structures foreach {
        case Left(action: LogicAction) => actions += action
        case Right(axiom: BoolExp) => axioms += axiom
      }
      Domain(predicates, actions.toList, axioms.toList, name)
  }

  lazy val predicatesDef = ("(:" ~> "predicates") ~> (rep(predicate) <~ ")")

  lazy val predicate = "(" ~> ident <~ ")" ^^ {
    Symbol(_)
  }

  lazy val structureDef = actionDef ^^ {
    Left(_)
  } | axiomDef ^^ {
    Right(_)
  }

  lazy val actionDef = ("(:" ~> "action" ~> ident) ~ (actionBody <~ ")") ^^ {
    case name ~ body => LogicAction(body._1, body._2, name)
  }

  lazy val actionBody = (":" ~> "precondition" ~> logicExpr) ~ (":" ~> "effect" ~> logicExpr) ^^ {
    case init ~ goal => (init, goal)
  }

  lazy val axiomDef = ("(:" ~> "axiom") ~> (logicExpr <~ ")")

  lazy val logicExpr = "(" ~> formula <~ ")"

  lazy val problem = ("(" ~> "define" ~> "(" ~> "problem" ~> ident <~ ")") ~ ("(:" ~> "domain" ~> ident <~ ")") ~ initDef ~ goalDef <~ ")" ^^ {
    case name ~ domainName ~ init ~ goal => UnboundProblem(domainName, init, goal, name)
  }

  lazy val initDef = ("(:" ~> "init") ~> logicExpr <~ ")"

  lazy val goalDef = ("(:" ~> "goal") ~> logicExpr <~ ")"

  lazy val pddl = domain ~ problem ^^ {
    case d ~ p => Problem(p, d)
  }

  /**
   * Copied from BooleanFormulaParserCombinator
   */

  lazy val formula: PackratParser[BoolExp] = term ~ ("&" ~> formula) ^^ {
    case f1 ~ f2 => f1 & f2
  } | term ~ ("|" ~> formula) ^^ {
    case f1 ~ f2 => f1 | f2
  } | term ~ ("->" ~> formula) ^^ {
    case f1 ~ f2 => f1 implies f2
  } | term ~ ("<->" ~> formula) ^^ {
    case f1 ~ f2 => f1 iff f2
  } | term

  lazy val term = "~" ~> "(" ~> formula <~ ")" ^^ {
    case f => f.unary_~()
  } | "(" ~> formula <~ ")" | lit

  lazy val lit: PackratParser[BoolExp] = "~" ~> ident ^^ {
    case s => Symbol(s).unary_~()
  } | unknown | ident ^^ {
    case s => Symbol(s): BoolExp
  }

  lazy val unknown = ident <~ "?" ^^ {
    case s => Symbol(s) | ~Symbol(s): BoolExp
  }
}

private[importexport] class PDDLLexical extends StdLexical {
  delimiters +=("(", "(:", ")", "=", "&", "|", "~", "->", "<->", "?", ":")
  reserved +=("define", "domain", "predicates", "actions", "axiom", "problem", "init",
    "goal", "action", "effect", "precondition")

  override def whitespace: Parser[Any] = rep(whitespaceChar | comment)

  protected override def comment: Parser[Any] = ';' ~ rep(chrExcept(EofCh, '\n'))

  override def identChar = super.identChar | elem('-')
}
