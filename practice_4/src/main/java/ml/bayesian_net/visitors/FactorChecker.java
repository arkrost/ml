package ml.bayesian_net.visitors;

import ml.bayesian_net.Factor;

public class FactorChecker implements NetChecker {
    private boolean failed;

    @Override
    public boolean isFailed() {
        return failed;
    }

    private boolean check(int v, Factor factor, boolean[][] links, int[] vars) {
        for (int u = 0; u < vars.length; u++) {
            if (links[u][v] || v == u) {
                if (!factor.isSubMask(vars[u]))
                    return true;
            } else if (factor.isSubMask(vars[u])) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void visit(Factor[] factors, boolean[][] links, int[] vars) {
        failed = false;
        for (int v = 0; v < factors.length; v++) {
            if (check(v, factors[v], links, vars)) {
                failed = true;
                break;
            }
        }
    }
}
