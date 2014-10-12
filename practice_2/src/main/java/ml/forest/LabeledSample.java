package ml.forest;

/**
 * @author Arkadii Rost
 */
public class LabeledSample {
    private final int[] sample;
    private final int label;

    public LabeledSample(int[] sample, int label) {
        this.sample = sample;
        this.label = label;
    }

    public int getLabel() {
        return label;
    }

    public int[] getSample() {
        return sample;
    }
}
