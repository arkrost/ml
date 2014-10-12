package ml.forest.tree;

/**
 * @author Arkadii Rost
 */
public class Branch extends Node {
    private final int featureID;
    private final double c;
    private final Node left;
    private final Node right;

    public Branch(int featureID, double c, Node left, Node right) {
        this.featureID = featureID;
        this.c = c;
        this.left = left;
        this.right = right;
    }

    @Override
    public int resolve(int[] sample) {
        if (sample[featureID] <= c)
            return left.resolve(sample);
        return right.resolve(sample);
    }
}
