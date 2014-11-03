package ml.pca;

import org.ejml.simple.SimpleMatrix;
import org.ejml.simple.SimpleSVD;

import java.io.IOException;
import java.util.Scanner;

/**
 * @author Arkadii Rost
 */
public class Main {
    private static final String BASIS1 = "newBasis1";
    private static final String BASIS2 = "newBasis2";
    private static final String BASIS3 = "newBasis3";

    private static final int COLUMN_COUNT = 200;
    private static final int ROW_COUNT = 1000;

    public static void main(String[] args) {
        double[][] matrix = new double[][] {
                {7, 4, 3},
                {4, 1, 8},
                {6, 3, 5},
                {8, 6, 1},
                {8, 5, 7},
                {7, 2, 9},
                {5, 3, 3},
                {9, 5, 8},
                {7, 4, 5},
                {8, 2, 2}
        };
        System.out.println(pca(new SimpleMatrix(matrix)));
    }

    private static SimpleMatrix readMatrix(String path) {
        SimpleMatrix matrix = new SimpleMatrix(ROW_COUNT, COLUMN_COUNT);
        try (Scanner sc = new Scanner(Main.class.getResource(path).openStream())) {
            for (int i = 0; i < ROW_COUNT; i++) {
                for (int j = 0; j < COLUMN_COUNT; j++)
                    matrix.set(i, j, sc.nextDouble());
            }
            if (sc.hasNext())
                throw new RuntimeException("Something left in file");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return matrix;
    }

    private static SimpleMatrix cov(SimpleMatrix matrix) {
        return matrix.transpose().mult(matrix);
    }

    private static SimpleMatrix slim(SimpleMatrix w, int l) {
        return w.extractMatrix(0, w.numRows(), 0, l);
    }

    private static SimpleMatrix pca(SimpleMatrix m) {
        SimpleMatrix x = new SimpleMatrix(m);
        center(x);
        norm(x);
        SimpleMatrix c = cov(x).scale(1.0 / (m.numRows() - 1));
        SimpleSVD svd = c.svd();
        int l = 3;//selectL(svd.getW());
        return x.mult(slim(svd.getV(), l));
    }

    private static void center(SimpleMatrix m) {
        for (int j = 0; j < m.numCols(); j++) {
            double avg = 0;
            for (int i = 0; i < m.numRows(); i++)
                avg += m.get(i, j) / m.numRows();
            for (int i = 0; i < m.numRows(); i++)
                m.set(i, j, m.get(i, j) - avg);
        }
    }

    private static void norm(SimpleMatrix m) {
        for (int j = 0; j < m.numCols(); j++) {
            double w = 0;
            for (int i = 0; i < m.numRows(); i++)
                w += m.get(i, j) * m.get(i, j);
            w = Math.sqrt(w / (m.numRows() - 1));
            for (int i = 0; i < m.numRows(); i++)
                m.set(i, j, m.get(i, j) / w);
        }
    }

    private static int selectL(SimpleMatrix w) {
        double avgLambda  = w.trace() / w.numRows();
        int i = 0;
        while (w.get(i, i) > avgLambda)
            i++;
        return i;
    }
}
