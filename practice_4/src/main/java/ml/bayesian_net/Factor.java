package ml.bayesian_net;

import java.util.HashMap;
import java.util.Map;

public class Factor {
    private final int varMask;
    private final Map<Integer, Double> mapping;

    public Factor(int mask, Map<Integer, Double> mapping) {
        this.varMask = mask;
        this.mapping = mapping;
    }

    public double get(int vm) {
        return mapping.get(vm & varMask);
    }

    public boolean isSubMask(int mask) {
        return (mask | varMask) == varMask;
    }

    public boolean isConstant() {
        return varMask == 0;
    }

    public Factor marginal(int mask) {
        if (!isSubMask(mask))
            throw new IllegalStateException("Can't marginal: not sub mask");
        Map<Integer, Double> newMapping = new HashMap<>(1 << Integer.bitCount(mask));
        for (Map.Entry<Integer, Double> e : mapping.entrySet()) {
            int ind = e.getKey() ^ (e.getKey() & mask);
            newMapping.put(ind, e.getValue() + newMapping.getOrDefault(ind, 0.));
        }
        return new Factor(varMask ^ mask, newMapping);
    }

    public Factor compose(Factor f) {
        int mask = varMask | f.varMask;
        int common = varMask & f.varMask;
        Map<Integer, Double> newMapping = new HashMap<>(1 << Integer.bitCount(mask));
        for (Map.Entry<Integer, Double> em : mapping.entrySet()) {
            //noinspection Convert2streamapi
            for (Map.Entry<Integer, Double> ef : f.mapping.entrySet()) {
                if ((ef.getKey() & common) == (em.getKey() & common)) {
                    int ind = ef.getKey() | em.getKey();
                    newMapping.put(ind, ef.getValue() * em.getValue());
                }
            }
        }
        return new Factor(mask, newMapping);
    }

    public Factor fix(int subMask, int val) {
        subMask &= varMask;
        if (subMask == 0)
            return this;
        val &= varMask;
        Map<Integer, Double> newMapping = new HashMap<>();
        for (Map.Entry<Integer, Double> e : mapping.entrySet()) {
            int ind = e.getKey() & subMask;
            if (ind == val)
                newMapping.put(e.getKey() ^ ind, e.getValue());
        }
        return new Factor(varMask ^ subMask, newMapping);
    }
}
