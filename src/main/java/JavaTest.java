import com.glowingavenger.agent.plan.ContingencyPlan;
import com.glowingavenger.agent.problem.AskAction;
import com.glowingavenger.agent.problem.Problem;
import com.glowingavenger.agent.util.ActionEdge;
import com.glowingavenger.agent.util.PlanDescr;
import com.glowingavenger.agent.util.PlanExport;
import org.jgrapht.DirectedGraph;

import java.util.HashMap;
import java.util.Map;

public class JavaTest {
    public static void main(String[] args) {
        Problem lampProblem = Problem.lampProblem();
        ContingencyPlan planner = ContingencyPlan.apply(lampProblem);
        PlanDescr description = PlanExport.exportDescription(planner.build());
        DirectedGraph<Map<String, Boolean>, ActionEdge> plan = description.plan();

        Map<String, Boolean> goal = description.goal();
        if (goal == null) {
            System.out.println("The goal is unreachable!");
        }

        // Maps ask actions to corresponding values of attributes (true or false)
        Map<AskAction, Boolean> askActions = new HashMap<AskAction, Boolean>();
        for (ActionEdge edge : plan.outgoingEdgesOf(description.init())) {
            if (edge.action() instanceof AskAction) {
                AskAction a = (AskAction) edge.action();
                Boolean attrValue = edge.to().get(a.attr().name());
                askActions.put(a, attrValue);
            }
        }
    }
}
