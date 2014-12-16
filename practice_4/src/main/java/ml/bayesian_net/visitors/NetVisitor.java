package ml.bayesian_net.visitors;

import ml.bayesian_net.Factor;

public interface NetVisitor {
    void visit(Factor[] factors, boolean[][] links, int[] vars);
}
