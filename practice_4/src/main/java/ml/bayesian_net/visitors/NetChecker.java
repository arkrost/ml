package ml.bayesian_net.visitors;

public interface NetChecker extends NetVisitor {
    boolean isFailed();
}
