package com.glowingavenger.agent;

import com.glowingavenger.plan.problem.Action;
import com.glowingavenger.plan.util.ActionEdge;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;

public interface PlanStateChangeListener {
    ActionEdge onStateChange(@Nullable Map<String, Boolean> stateBefore, Map<String, Boolean> state, @Nullable ActionEdge action, Iterable<ActionEdge> successors);

    void onFinish(Map<String, Boolean> state, @Nullable ActionEdge action);

    void onFailure(Map<String, Boolean> state, @Nullable ActionEdge action);
}
