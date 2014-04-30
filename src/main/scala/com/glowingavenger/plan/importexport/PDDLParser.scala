package com.glowingavenger.plan.importexport

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import org.sat4j.scala.Logic.{True, BoolExp}
import org.sat4j.scala.Logic.identFromSymbol
import com.glowingavenger.plan.problem.LogicAction
import scala.util.parsing.combinator.PackratParsers
import scala.io.Source
import scala.util.parsing.input.CharArrayReader

case class Domain(name: String, predicates: Map[Symbol, String], actions: Map[LogicAction, String], kbase: List[BoolExp])

case class Problem(name: String, domain: String, init: BoolExp, goal: BoolExp)

case class PDDLDescription(domain: Domain, problem: Problem)

object PDDLParser extends StandardTokenParsers with PackratParsers {

  case class DomainStub(name: String, predicates: Map[Symbol, String], actions: Map[String, String], kbase: List[BoolExp])

  override val lexical = new PDDLLexical

  lazy val domain = ("(" ~> "define" ~> "(" ~> "domain" ~> ident <~ ")") ~ (rep(compositeAttr) ~ opt(kbase) <~ ")") ^^ {
    case name ~ attrs => attrs match {
      case attrList ~ Some(kb) => attrList match {
        case List(("predicates", Some(predicateAttrs)), ("actions", Some(actionList))) =>
          val predicateMap = (predicateAttrs map (p => (Symbol(p._1), p._2))).toMap
          Some(DomainStub(name, predicateMap, actionList.toMap, kb))
        case _ => None
      }
      case _ => None
    }
    case _ => None
  }

  lazy val compositeAttr = "(:" ~> ("predicates" | "actions") ~ opt(rep(attr)) <~ ")" ^^ {
    case attrName ~ attrList => (attrName, attrList)
  }

  lazy val predicates = "(:" ~> "predicates" ~> opt(rep(attr)) <~ ")"

  lazy val actions = "(:" ~> "actions" ~> opt(rep(attr)) <~ ")"

  lazy val kbase = ("(:" ~> "kbase") ~> opt(rep(formula)) <~ ")" ^^ {
    case Some(list) => list filter {
      case expr: BoolExp => true
      case _ => false
    } map {
      case expr: BoolExp => expr
      case _ => throw new IllegalArgumentException
    }
    case _ => Nil
  }

  lazy val attr = ("(" ~> ident) ~ ("=" ~> stringLit <~ ")") ^^ (a => (a._1, a._2))

/*  lazy val kbaseAttr = "(" ~> stringLit <~ ")" ^^ {
    s =>
      formula(new PackratReader(new BooleanFormulaParserCombinator.lexical.Scanner(s))) match {
        // TODO Hacked the parser here so it returns Symbol instead of Strings as Idents. This should be fixed
        // The problem is that encode() returns two more anonymous variables out of nowhere
        case BooleanFormulaParserCombinator.Success(f: BoolExp, _) => Some(f)
        case _ => println(s"failed to parse formula $s"); None
      }
  }*/

  lazy val problem = ("(" ~> "define" ~> "(" ~> "problem" ~> ident <~ ")") ~ (domainAttr ~ rep(problemKBaseAttr) <~ ")") ^^ {
    case name ~ attrs => attrs match {
        case Some(domainName) ~ List(Some(("init", init)), Some(("goal", goal))) =>
          Some(Problem(name, domainName, init, goal))
        case _ => None
      }
    case _ => None
  }

  lazy val domainAttr = "(:" ~> "domain" ~ ident <~ ")" ^^ {
    case "domain" ~ domainName => Some(domainName)
    case _ => None
  }

  lazy val problemKBaseAttr = "(:" ~> ("init" | "goal") ~ formula <~ ")" ^^ {
    case name ~ expr => Some((name, expr))
    case _ => None
  }

  lazy val definition = "(:" ~> defIdent ~ rep(definitionAttr) <~ ")" ^^ {
    case defn ~ attrs =>
      defn match {
        case (dname, name: String) if dname == "action" => attrs match {
          case List((":precond", precond), (":effect", effect)) => Some(LogicAction(precond, effect, name))
          case List((":effect", effect)) => Some(LogicAction(True, effect, name))
          case _ => None
        }
      }
    case _ => None
  }

  lazy val defIdent: Parser[Any] = "action" ~ ident ^^ (a => (a._1, a._2))

  lazy val definitionAttr = defAttrKeyword ~ formula ^^ (a => (a._1, a._2))

  lazy val defAttrKeyword = ":effect" | ":precond"

  lazy val dsl = domain ~ problem ~ rep(definition) ^^ {
    case Some(d) ~ Some(p) ~ list =>
      val actions = list filter (_.isDefined) map { case Some(a) => a }
      val actionMap = d.actions map { case (k, v) => (actions.find(_.name == k), v) }
      actionMap.find(!_._1.isDefined) match {
        case None => Some(PDDLDescription(Domain(d.name, d.predicates,
          actionMap map (p => (p._1.get, p._2)), d.kbase), p))
        case _ => None
      }
  }

  /**
   * Copied from BooleanFormulaParserCombinator
   */

  val formula: PackratParser[BoolExp] =  term ~ ("&" ~> formula) ^^ {
    case f1 ~ f2 => f1 & f2
  } | term ~ ("|" ~> formula) ^^ {
    case f1 ~ f2 => f1 | f2
  } | term ~ ("->" ~> formula) ^^ {
    case f1 ~ f2 => f1 implies f2
  } | term ~ ("<->" ~> formula) ^^ {
    case f1 ~ f2 => f1 iff f2
  } | term

  val term =  "~" ~> "(" ~> formula <~ ")" ^^ {
    case f  => f.unary_~()
  } | "(" ~> formula <~ ")" | lit

  val lit: PackratParser[BoolExp] =  "~" ~> ident ^^ {
    case s => Symbol(s).unary_~()
  } | ident ^^ {
    case s => Symbol(s): BoolExp
  }

  def main(args: Array[String]) {
    /*println(attr(new PackratReader(new lexical.Scanner("(L=\"Лампа горит\")"))))
    println(kbaseAttr(new PackratReader(new lexical.Scanner("(\"L <-> B & S\")"))))
    println(kbase(new PackratReader(new lexical.Scanner("(:kbase (\"L <-> B & S\"))"))))
    println(actions(new PackratReader(new lexical.Scanner("(:actions\n\t\t(off_light=\"Выключить выключатель\") (on_light=\"Включить выключатель\") (ch_bulb=\"Заменить лампу\") (off_cond=\"Выключить кондиционер\") (on_cond=\"Включить кондиционер\") (off_wind=\"Закрыть окно\") (on_wind=\"Открыть окно\"))"))))
    println(predicates(new PackratReader(new lexical.Scanner("(:predicates\n\t\t(L=\"Лампа горит\") (B=\"Лампа исправна\") (S=\"Выключатель включен\") (C=\"Кондиционер включен\") (W=\"Окно открыто\"))"))))
    println(domain(new PackratReader(new lexical.Scanner("(define (domain bulb)\n\t(:predicates\n\t\t(L=\"Лампа горит\")(B=\"Лампа исправна\")(S=\"Выключатель включен\")(C=\"Кондиционер включен\")(W=\"Окно открыто\")\n\t)\n\t(:actions\n\t\t(off_light=\"Выключить выключатель\")(on_light=\"Включить выключатель\")(ch_bulb=\"Заменить лампу\")(off_cond=\"Выключить кондиционер\")(on_cond=\"Включить кондиционер\")(off_wind=\"Закрыть окно\")(on_wind=\"Открыть окно\"))\n\t(:kbase (\"L <-> B & S\"))\n)"))))
    println(domainAttr(new PackratReader(new lexical.Scanner("(:domain bulb)"))))
    println(problemKBaseAttr(new PackratReader(new lexical.Scanner("(: init (\"~L\"))"))))
    println(problem(new PackratReader(new lexical.Scanner("(define (problem switch_on)\n\t(: domain bulb)\n\t(: init (\"~L\"))\n\t(: goal (\"L\")))"))))
    println(definitionAttr(new PackratReader(new lexical.Scanner(":effect (\"~S\")"))))
    println(definition(new PackratReader(new lexical.Scanner("(: action ch_bulb :precond (\"~S\") :effect (\"B\"))"))))
    println(dsl(new PackratReader(new lexical.Scanner("(define (domain bulb)\n\t(:predicates\n\t\t(L=\"Лампа горит\")(B=\"Лампа исправна\")(S=\"Выключатель включен\")(C=\"Кондиционер включен\")(W=\"Окно открыто\")\n\t)\n\t(:actions\n\t\t(off_light=\"Выключить выключатель\")(on_light=\"Включить выключатель\")(ch_bulb=\"Заменить лампу\")(off_cond=\"Выключить кондиционер\")(on_cond=\"Включить кондиционер\")(off_wind=\"Закрыть окно\")(on_wind=\"Открыть окно\"))\n\t(:kbase (\"L <-> B & S\"))\n) (define (problem switch_on)\n\t(:domain bulb)\n\t(:init (\"~L\"))\n\t(:goal (\"L\"))) (: action ch_bulb :precond (\"~S\") :effect (\"B\"))"))))*/
    val str = Source.fromFile("src/main/resources/Bulb.pddl").mkString
    println(dsl(new PackratReader(new lexical.Scanner(str))))
  }

}

class PDDLLexical extends StdLexical {
  delimiters +=("(", "(:", ")", "=", "&", "|", "~", "->", "<->", ";")
  reserved +=("define", "domain", "predicates", "actions", "kbase", "problem", "init", "goal", "action", ":effect", ":precond")

  override def identChar = super.identChar | elem(':')
}
