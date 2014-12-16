package ml.bayesian_net.visitors;

import ml.bayesian_net.Factor;

public class GraphvizVizitor implements NetVisitor {
    private static final String DIGRAPH = "digraph";
    private final String graphName;

    private String digraph;

    public String getDigraph() {
        return digraph;
    }

    public GraphvizVizitor(String graphName) {
        this.graphName = graphName;
    }

    @Override
    public void visit(Factor[] factors, boolean[][] links, int[] vars) {
        StringBuilder builder = new StringBuilder(DIGRAPH).append(" ");
        builder.append(graphName).append(" {\n");
        for (int u = 0; u < links.length; u++) {
            for (int v = 0; v < links[u].length; v++) {
                if (links[u][v])
                    builder.append(String.format("\t%d -> %d;\n", u, v));
            }
        }
        builder.append("}");
        digraph = builder.toString();
    }

}
