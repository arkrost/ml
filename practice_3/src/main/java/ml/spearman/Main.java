package ml.spearman;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;
import ml.PlotterPane;
import ml.forest.Classifier;
import ml.forest.Forest;
import ml.forest.LabeledSample;

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
public class Main extends Application {
    private static final String TRAIN_DATA = "arcene_train.data";
    private static final String TRAIN_LABELS = "arcene_train.labels";
    private static final String TEST_DATA = "arcene_valid.data";
    private static final String TEST_LABELS = "arcene_valid.labels";
    private static final int MAX_DEPTH = 10;
    private static final int FOREST_SIZE = 51;
    private static final int SAMPLES_COUNT = 100;
    private static final int MIN_FEATURE_SIZE = 3;
    private static final int MAX_FEATURE_SIZE = 300;
    private static final int FEATURE_STEP = 3;
    private static final int T = 5;

    private final double[] trainPoints;
    private final double[] testPoints;

    public Main() {
        List<LabeledSample> trainData = readLabeledSamples(TRAIN_DATA, TRAIN_LABELS);
        List<LabeledSample> testData = readLabeledSamples(TEST_DATA, TEST_LABELS);
        Filter filter = new Filter(trainData);
        int size = 2 * (MAX_FEATURE_SIZE - MIN_FEATURE_SIZE);
        trainPoints = new double[size];
        testPoints = new double[size];
        for (int i = 0; i < MAX_FEATURE_SIZE - MIN_FEATURE_SIZE; i += FEATURE_STEP) {
            int count = i + MIN_FEATURE_SIZE;
            trainPoints[2 * i] = count;
            List<LabeledSample> filteredTest = filter.take(testData, count);
            testPoints[2 * i] = count;
            List<LabeledSample> filteredTrain = filter.take(trainData, count);
            for (int j = 0; j < T; j ++) {
                System.out.println(i + " " + j);
                Forest forest = new Forest(FOREST_SIZE, SAMPLES_COUNT, MAX_DEPTH, filteredTrain);
                trainPoints[2 * i + 1] += testClassifier(forest, filteredTrain) / T;
                testPoints[2 * i + 1] += testClassifier(forest, filteredTest) / T;
            }
        }
    }

    public static void main(String[] args) {
       Application.launch(Main.class);
    }

    @Override
    public void start(Stage stage) throws Exception {
        PlotterPane charts = new PlotterPane();
        charts.addChart(trainPoints, "On train data");
        charts.addChart(testPoints, "On test data");
        stage.setTitle("Filtering charts");
        stage.setScene(new Scene(charts));
        stage.sizeToScene();
        stage.show();
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

    private static Path getPath(String path) {
        return Paths.get(Main.class.getResource(path).getPath());
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

    private static List<LabeledSample> readLabeledSamples(String dataPath, String labelsPath) {
        List<int[]> data = readSamples(getPath(dataPath));
        List<Integer> labels = readLabels(getPath(labelsPath));
        List<LabeledSample> samples = new ArrayList<>(data.size());
        for (int i = 0; i < data.size(); i++)
            samples.add(new LabeledSample(data.get(i), labels.get(i)));
        return samples;
    }

    private static  double testClassifier(Classifier classifier, List<LabeledSample> samples) {
        int count = 0;
        for (LabeledSample sample : samples) {
            if (classifier.resolve(sample.getSample()) == sample.getLabel())
                count++;
        }
        return  100. * count / samples.size();
    }
}
