package com.glowingavenger.plan.importexport

import com.glowingavenger.plan.ContingencyPlan
import scala.io.Source
import org.jgrapht.ext._
import com.glowingavenger.plan.ActionEdge
import java.io.{File, PrintWriter}
import com.glowingavenger.plan.model.state.BeliefState
import scala.sys.process._

object PlanExport {
  def main(args: Array[String]) {
    require(args.length == 2, "Invalid number of arguments")

    val input = args(0)
    val output = args(1)

    println("Generating the plan...")
    val plan = ContingencyPlan(PDDL.importProblem(Source.fromFile(input).mkString))
    val dotExport = new DOTExporter[BeliefState, ActionEdge](new IntegerNameProvider[BeliefState],
      new StringNameProvider[BeliefState], new StringEdgeNameProvider[ActionEdge])

    val dotFile = new File(output)
    dotFile.delete()
    dotExport.export(new PrintWriter(new File(output)), plan.plan)
    println("Exported to DOT format successfully. Invoking Graphviz `dot` to create a PNG image.")
    val imgFile = new File(dotFile.getParentFile, dotFile.getName.replaceAll("\\.[^.]*$", "") + ".png")
    imgFile.delete()
    s"dot -Tpng ${dotFile.getAbsolutePath} -o ${imgFile.getAbsolutePath}".!
  }
}
