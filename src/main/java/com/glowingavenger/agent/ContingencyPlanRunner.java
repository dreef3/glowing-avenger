package com.glowingavenger.agent;

import com.glowingavenger.plan.ContingencyPlan;
import com.glowingavenger.plan.problem.Action;
import com.glowingavenger.plan.problem.Problem;
import com.glowingavenger.plan.util.ActionEdge;
import com.glowingavenger.plan.util.PlanDescr;
import com.glowingavenger.plan.util.PlanExport;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class ContingencyPlanRunner implements Runnable {
    private final ContingencyPlan planner;
    private final Problem problem;
    private final PlanStateChangeListener listener;
    private final PlanDescr plan;

    public ContingencyPlanRunner(@NotNull Problem problem, PlanStateChangeListener listener) {
        this.listener = listener;
        this.problem = problem;
        this.planner = ContingencyPlan.apply(problem);
        this.plan = PlanExport.exportDescription(planner.build());
    }

    @Override
    public void run() {
        Map<String, Boolean> state = plan.init();
        Map<String, Boolean> beforeState = null;
        ActionEdge producer = null;

        while (!state.equals(plan.goal())) {
            Set<ActionEdge> successors = plan.plan().outgoingEdgesOf(state);
            if (successors.isEmpty()) {
                listener.onFailure(state, producer);
                return;
            }

            // TODO The client shouldn't be able to instantiate Action objects
            producer = listener.onStateChange(beforeState, state, producer, successors);
            beforeState = state;
            state = producer.to();
        }

        listener.onFinish(state, producer);
    }
}
