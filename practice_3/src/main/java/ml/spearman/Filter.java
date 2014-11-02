package ml.spearman;

import ml.forest.LabeledSample;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Arkadii Rost
 */
public class Filter {
    private final List<Integer> order;

    public Filter(List<LabeledSample> samples) {
        int featureCount = samples.get(0).getSample().length;
        order = new ArrayList<>(featureCount);
        for (int i = 0; i < featureCount; i++)
            order.add(i);
        Collections.sort(order, (i, j) -> Double.compare(spearman(samples, i), spearman(samples, j)));
    }

    public List<LabeledSample> take(List<LabeledSample> samples, int count) {
        List<Integer> features = order.subList(0, count);
        return samples.stream()
                .map(s -> new LabeledSample(trimSample(s.getSample(), features), s.getLabel()))
                .collect(Collectors.toList());
    }

    private int[] trimSample(int[] sample, List<Integer> features) {
        int[] res = new int[features.size()];
        int j = 0;
        for (int f : features)
            res[j++] = sample[f];
        return res;
    }

    private double getXAvg(List<LabeledSample> samples, int j) {
        return samples.stream().mapToDouble(s -> s.getSample()[j]).average().getAsDouble();
    }

    private double spearman(List<LabeledSample> samples, int j) {
        double yAvg = samples.stream().mapToDouble(LabeledSample::getLabel).average().getAsDouble();
        double xAvg = getXAvg(samples, j);
        double xs = 0;
        double ys = 0;
        double numer = 0;
        for (LabeledSample sample : samples) {
            double dx = sample.getSample()[j] - xAvg;
            double dy = sample.getLabel() - yAvg;
            xs += dx * dx;
            ys += dy * dy;
            numer += dx * dy;
        }
        return numer / Math.sqrt(xs * ys);
    }
}
