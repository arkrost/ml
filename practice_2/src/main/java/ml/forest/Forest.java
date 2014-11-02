package ml.forest;

import ml.forest.tree.Node;

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

/**
 * @author Arkadii Rost
 */
public class Forest implements Classifier {
    private final Node[] trees;

    public Forest(int size, int m, int depth, List<LabeledSample> samples) {
        trees = new Node[size];
        Random random = new Random();
        for (int i = 0; i < size; i++)
            trees[i] = Node.buildTree(random, genRandomSamples(samples, m, random), depth);
    }

    private static List<LabeledSample> genRandomSamples(List<LabeledSample> samples,
            int m, Random random)
    {
        LabeledSample[] result = new LabeledSample[m];
        for (int i = 0; i < m; i++)
            result[i] = samples.get(random.nextInt(samples.size()));
        return Arrays.asList(result);
    }

    @Override
    public int resolve(int[] sample) {
        return Arrays.stream(trees)
                .collect(Collectors.groupingBy(n -> n.resolve(sample)))
                .entrySet().stream()
                .max((e1, e2) -> Integer.compare(e1.getValue().size(), e2.getValue().size()))
                .get().getKey();
    }
}
