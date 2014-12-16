package ml.bayesian_net.visitors;

import ml.bayesian_net.Factor;

import java.util.Arrays;

public class DAGChecker implements NetChecker {
   enum Colors {BLACK, WHITE, GRAY}

    private Colors[] colors;
    private boolean failed;

    private void dfs(int u, boolean[][] edges) {
        colors[u] = Colors.GRAY;
        for (int v = 0; v < colors.length; v++) {
            if (!edges[u][v])
                continue;
            if (colors[v] == Colors.GRAY) {
                failed = true;
                return;
            } else if (colors[v] == Colors.WHITE) {
                dfs(v, edges);
            }
        }
        colors[u] = Colors.BLACK;
    }

    public boolean isFailed() {
        return failed;
    }

    @Override
    public void visit(Factor[] factors, boolean[][] links, int[] vars) {
        failed = false;
        colors = new Colors[factors.length];
        Arrays.fill(colors, Colors.WHITE);
        for (int i = 0; i < colors.length; i++) {
            if (colors[i] == Colors.WHITE)
                dfs(i, links);
            if (failed)
                break;
        }
    }
}
