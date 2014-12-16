package ml.bayesian_net.visitors;

import ml.bayesian_net.Factor;

public class LinkChecker implements NetChecker {
    private boolean[] colors;
    private boolean failed;

    @Override
    public boolean isFailed() {
        return failed;
    }

    private void dfs(int u, boolean[][] edges) {
        colors[u] = true;
        for (int v = 0; v < colors.length; v++) {
            if ((edges[u][v] || edges[v][u]) && !colors[v])
                dfs(v, edges);
        }
    }

    @Override
    public void visit(Factor[] factors, boolean[][] links, int[] vars) {
        failed = false;
        colors = new boolean[factors.length];
        dfs(0, links);
        for (boolean visited : colors) {
            if (!visited) {
                failed = true;
                break;
            }
        }
    }
}
