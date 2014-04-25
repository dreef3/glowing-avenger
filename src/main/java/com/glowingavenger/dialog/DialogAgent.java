package com.glowingavenger.dialog;

import com.glowingavenger.agent.ContingencyPlanRunner;
import com.glowingavenger.agent.PlanStateChangeListener;
import com.glowingavenger.plan.problem.AskAction;
import com.glowingavenger.plan.problem.LogicAction;
import com.glowingavenger.plan.problem.Problem;
import com.glowingavenger.plan.util.ActionEdge;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DialogAgent implements PlanStateChangeListener {

    public static void main(String[] args) {
        new ContingencyPlanRunner(Problem.lampProblem(), new DialogAgent()).run();
    }

    private class Tuple<K, V> {
        public Tuple(K one, V two) {
            this.one = one;
            this.two = two;
        }

        public K one;
        public V two;
    }

    @Override
    public ActionEdge onStateChange(@Nullable Map<String, Boolean> stateBefore, Map<String, Boolean> state,
                                    @Nullable ActionEdge action, Iterable<ActionEdge> successors) {
        Map<AskAction, Tuple<ActionEdge, ActionEdge>> askActions = new HashMap<>();
        Map<LogicAction, ActionEdge> realActions = new HashMap<>();

        for (ActionEdge sc : successors) {
            if (sc.action() instanceof AskAction) {
                if (askActions.containsKey(sc.action())) {
                    askActions.put((AskAction) sc.action(), new Tuple<>(askActions.get(sc.action()).one, sc));
                } else {
                    askActions.put((AskAction) sc.action(), new Tuple<ActionEdge, ActionEdge>(sc, null));
                }
            } else if (sc.action() instanceof LogicAction) {
                realActions.put((LogicAction) sc.action(), sc);
            }
        }

        if (!askActions.isEmpty()) {
            for (Map.Entry<AskAction, Tuple<ActionEdge, ActionEdge>> entry : askActions.entrySet()) {
                int answer = JOptionPane.showConfirmDialog(null,
                        String.format("Is it true? %s\nPress cancel if you don't know", entry.getKey().attr().name()),
                        "Question", JOptionPane.YES_NO_CANCEL_OPTION);
                if (answer == JOptionPane.CANCEL_OPTION) {
                    continue;
                }

                if (answer == JOptionPane.YES_OPTION) {
                    if (entry.getValue().one.to().get(entry.getKey().attr().name())) {
                        return entry.getValue().one;
                    }
                    return entry.getValue().two;
                } else {
                    if (!entry.getValue().one.to().get(entry.getKey().attr().name())) {
                        return entry.getValue().one;
                    }
                    return entry.getValue().two;
                }
            }
        }

        ActionEdge first = realActions.values().iterator().next();
        JOptionPane.showMessageDialog(null,
                String.format("Please perform the action: %s", first.action().name()),
                "Action", JOptionPane.INFORMATION_MESSAGE);
        return first;
    }

    @Override
    public void onFinish(Map<String, Boolean> state, @Nullable ActionEdge action) {
        JOptionPane.showMessageDialog(null, "Hooray!");
    }

    @Override
    public void onFailure(Map<String, Boolean> state, @Nullable ActionEdge action) {
        JOptionPane.showMessageDialog(null, "Something went wrong...");
    }
}
