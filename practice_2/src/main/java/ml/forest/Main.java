package ml.forest;

import ml.forest.tree.Node;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Arkadii Rost
 */
public class Main {
    private static final Path DATA_ROOT = Paths.get("random_forest");
    private static final Path TRAIN_DATA = DATA_ROOT.resolve("arcene_train.data");
    private static final Path TRAIN_LABELS = DATA_ROOT.resolve("arcene_train.labels");
    private static final Path TEST_DATA = DATA_ROOT.resolve("arcene_valid.data");
    private static final Path TEST_LABELS = DATA_ROOT.resolve("arcene_valid.labels");
    private static final int MAX_DEPTH = 10;

    public static void main(String[] args) {
        List<LabeledSample> trainData = readLabeledSamples(TRAIN_DATA, TRAIN_LABELS);
        List<LabeledSample> testData = readLabeledSamples(TEST_DATA, TEST_LABELS);
        Node tree = Node.buildTree(trainData, MAX_DEPTH);
        System.out.printf("Correct percent on train data: %.2f\n", testClassifier(tree, trainData));
        System.out.printf("Correct percent on test data: %.2f\n", testClassifier(tree, testData));
    }

    private static List<int[]> readSamples(Path path) {
        List<int[]> res = new ArrayList<>();
        try (BufferedReader br = Files.newBufferedReader(path)) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] data = line.split(" ");
                int[] features = new int[data.length];
                for (int i = 0; i < data.length; i++)
                    features[i] = Integer.parseInt(data[i]);
                res.add(features);
            }
        } catch (IOException e) {
            throw new RuntimeException("Unexpected IO error", e);
        }
        return res;
    }

    private static List<Integer> readLabels(Path path) {
        List<Integer> res = new ArrayList<>();
        try (BufferedReader br = Files.newBufferedReader(path)) {
            String line;
            while ((line = br.readLine()) != null)
                res.add(Integer.valueOf(line));
        } catch (IOException e) {
            throw new RuntimeException("Unexpected IO error", e);
        }
        return res;
    }

    private static List<LabeledSample> readLabeledSamples(Path dataPath, Path labelsPath) {
        List<int[]> data = readSamples(dataPath);
        List<Integer> labels = readLabels(labelsPath);
        List<LabeledSample> samples = new ArrayList<>(data.size());
        for (int i = 0; i < data.size(); i++)
            samples.add(new LabeledSample(data.get(i), labels.get(i)));
        return samples;
    }

    private static  double testClassifier(Node node, List<LabeledSample> samples) {
        int count = 0;
        for (LabeledSample sample : samples) {
            if (node.resolve(sample.getSample()) == sample.getLabel())
                count++;
        }
        return  100. * count / samples.size();
    }
}
