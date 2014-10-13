package ml.forest.tree;

import ml.forest.Classifier;
import ml.forest.LabeledSample;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Arkadii Rost
 */
public abstract class Node implements Classifier {
    public static Node buildTree(List<LabeledSample> samples, int depth) {
        if (depth > 0 && shouldTrySplit(samples)) {
            double featureCount = samples.get(0).getSample().length;
            int id = -1;
            int split = -1;
            double gini = Double.NEGATIVE_INFINITY;
            for (int i = 0; i < featureCount; i++) {
                if (!canSplitByFeature(i, samples))
                    continue;
                int curSplit = splitInd(i, samples);
                double g = gini(samples.subList(0, curSplit),
                        samples.subList(curSplit, samples.size()));
                if (g > gini) {
                    gini = g;
                    id = i;
                    split = curSplit;
                }
            }
            if (id != -1) {
                samples.sort(new ByFeatureComparator(id));
                List<LabeledSample> left = samples.subList(0, split);
                List<LabeledSample> right = samples.subList(split, samples.size());
                double c = 0.5 * (getFeature(id, samples.get(split - 1))
                        + getFeature(id, samples.get(split)));
                return new Branch(id, c, buildTree(left, depth - 1), buildTree(right, depth - 1));
            }
        }
        return new Leaf(mostFrequentLabel(samples));
    }

    private static Map<Integer, List<LabeledSample>> groupByLabel(List<LabeledSample> samples) {
        return samples.stream().collect(Collectors.groupingBy(LabeledSample::getLabel));
    }

    private static int mostFrequentLabel(List<LabeledSample> samples) {
        return groupByLabel(samples)
                .entrySet().stream()
                .max((e1, e2) -> Integer.compare(e1.getValue().size(), e2.getValue().size()))
                .get().getKey();
    }

    private static boolean shouldTrySplit(List<LabeledSample> samples) {
        int label = samples.get(0).getLabel();
        for (LabeledSample sample : samples) {
            if (sample.getLabel() != label)
                return true;
        }
        return false;
    }

    private static boolean canSplitByFeature(int featureId, List<LabeledSample> samples) {
        int value = getFeature(featureId, samples.get(0));
        for (LabeledSample sample : samples) {
            if (getFeature(featureId, sample) != value)
                return true;
        }
        return false;
    }

    private static double gini(List<LabeledSample> left, List<LabeledSample> right) {
        return gini(left) + gini(right);
    }

    private static double gini(List<LabeledSample> samples) {
        double N = samples.size();
        return groupByLabel(samples).values().stream()
                      .mapToDouble(group -> group.size() * group.size() / N)
                      .sum();
    }

    private static int splitInd(int featureID, List<LabeledSample> samples) {
        samples.sort(new ByFeatureComparator(featureID));
        int half = samples.size() / 2;
        int s1 = half - 1;
        while (s1 >= 0 && getFeature(featureID, samples.get(s1))
                == getFeature(featureID, samples.get(half))) {
            s1--;
        }
        int s2 = half + 1;
        while (s2 < samples.size() - 1 && getFeature(featureID, samples.get(s2))
                == getFeature(featureID, samples.get(half))) {
            s2++;
        }
        return half - s1 <= s2 - half ? s1 + 1 : s2;
    }

    private static int getFeature(int id, LabeledSample sample) {
        return sample.getSample()[id];
    }

    private static class ByFeatureComparator implements Comparator<LabeledSample> {
        private final int featureId;

        private ByFeatureComparator(int featureId) {
            this.featureId = featureId;
        }

        @Override
        public int compare(LabeledSample first, LabeledSample second) {
            return Integer.compare(first.getSample()[featureId], second.getSample()[featureId]);
        }
    }
}
