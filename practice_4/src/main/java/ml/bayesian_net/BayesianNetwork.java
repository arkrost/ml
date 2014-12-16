package ml.bayesian_net;

import ml.bayesian_net.visitors.NetVisitor;

import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class BayesianNetwork {
    private final String[] names;
    private final boolean[][] links;
    private final Factor[] factors;
    private final int[] vars;

    public BayesianNetwork(String... names) {
        this.names = names;
        vars = new int[names.length];
        vars[0] = 1;
        for (int i = 1; i < vars.length; i++)
            vars[i] = vars[i - 1] << 1;
        links = new boolean[names.length][names.length];
        factors = new Factor[names.length];
    }

    public int getSize() {
        return names.length;
    }

    public int getMask(int i) {
        return vars[i];
    }

    public void addEdge(int from, int to) {
        links[from][to] = true;
    }

    public void visit(NetVisitor visitor) {
        visitor.visit(factors, links, vars);
    }

    public void setFactor(int i, Factor factor) {
        factors[i] = factor;
    }

    public double[] eliminate(int mask, int val) {
        double[] res = new double[vars.length];
        for (int i = 0; i < vars.length; i++) {
            if ((vars[i] & mask) == vars[i]) { //variable fixed
                res[i] = (vars[i] & val) == vars[i] ? 1 : 0;
            } else {
                res[i] = eliminate(i, mask, val);
            }
        }
        return res;
    }

    private double eliminate(int v, int mask, int val) {
        Set<Factor> factors = getFactorSet(mask, val);
        int fixedVars = mask;
        int freeVariables = vars.length - Integer.bitCount(mask) - 1;
        for (int i = 0; i < freeVariables; i++) { //eliminating free variables
            int u = minAffectingVariable(v, fixedVars, factors);
            fixedVars ^= vars[u];
            List<Factor> affected = affectedFactors(factors, u);
            if (affected.isEmpty())
                continue;
            Factor newFactor = fold(affected).marginal(vars[u]);
            factors.removeAll(affected);
            if (!newFactor.isConstant())
                factors.add(newFactor);
        }
        Factor res = fold(factors);
        double p = res.get(vars[v]);
        return p / (p + res.get(0));
    }

    private Set<Factor> getFactorSet(int mask, int val) {
        return Stream.of(this.factors)
                .map(f -> f.fix(mask, val))
                .filter(f -> !f.isConstant())
                .collect(Collectors.toSet());
    }

    private Factor fold(Iterable<Factor> factors) {
        Iterator<Factor> iterator = factors.iterator();
        Factor acc = iterator.next();
        if (!iterator.hasNext())
            return acc;
        while (iterator.hasNext())
            acc = acc.compose(iterator.next());
        return acc;
    }

    private List<Factor> affectedFactors(Set<Factor> factors, int u) {
        return factors.stream().filter(f -> f.isSubMask(vars[u]))
                      .collect(Collectors.toList());
    }

    private int countAffectedFactors(int v, Set<Factor> factors) {
        return (int)factors.stream().filter(f -> f.isSubMask(vars[v])).count();
    }

    private int minAffectingVariable(int v, int mask, Set<Factor> factors) {
        int minAffect = Integer.MAX_VALUE;
        int minInd = -1;
        for (int u = 0; u < vars.length; u++) {
            if (u == v || (mask & vars[u]) != 0)
                continue;
            int affect = countAffectedFactors(u, factors);
            if (affect < minAffect) {
                minAffect = affect;
                minInd = u;
            }
        }
        return minInd;
    }
}
