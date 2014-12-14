package ml.bayesian_net;

public class BayesianNetwork {
    private final String[] names;
    private final boolean[][] links;
    private final int[] vars;

    public BayesianNetwork(String... names) {
        this.names = names;
        vars = new int[names.length];
        vars[0] = 1;
        for (int i = 1; i < vars.length; i++)
            vars[i] = vars[i - 1] << 1;
        links = new boolean[names.length][names.length];
    }

    public void addEdge(int from, int to) {
        links[from][to] = true;
    }
}
