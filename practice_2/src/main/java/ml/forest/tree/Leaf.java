package ml.forest.tree;

/**
 * @author Arkadii Rost
 */
public class Leaf extends Node {
    private final int value;

    public Leaf(int value) {
        this.value = value;
    }

    @Override
    public int resolve(int[] sample) {
        return value;
    }
}
