package ml;

import ml.bayesian_net.BayesianNetwork;
import ml.bayesian_net.Factor;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

public class Main {
    public static void main(String[] args) {
        try (BufferedReader reader = Files.newBufferedReader(Paths.get(args[0]))) {
            String nextLine = reader.readLine();
            BayesianNetwork net = new BayesianNetwork(getElements(nextLine));
            while (!(nextLine = reader.readLine()).equals("end")) {
                String[] simpleFactor = nextLine.split(" : ");
                int vertex = Integer.parseInt(simpleFactor[0]);
                double prob = getProb(simpleFactor[1]);
                Map<Integer, Double> mapping = new HashMap<>(2);
                int varMask = net.getMask(vertex);
                mapping.put(varMask, prob);
                mapping.put(0, 1 - prob);
                net.setFactor(vertex, new Factor(varMask, mapping));
            }
            for (int i = 0; i < net.getSize(); i++) {
                nextLine = reader.readLine();
                int sep = getSeparator(nextLine);
                int u = Integer.parseInt(nextLine.substring(0, sep - 1));
                String[] vertexes;
                if (sep != nextLine.length() - 1) {
                    vertexes = getElements(nextLine.substring(sep + 2));
                    for (String v : vertexes)
                        net.addEdge(u, Integer.parseInt(v));
                }
                Map<Integer, Double> mapping = new HashMap<>();
                int mask = net.getMask(u);
                while (!(nextLine = reader.readLine()).equals("end")) {
                    sep = getSeparator(nextLine);
                    vertexes = getElements(nextLine.substring(0, sep - 1));
                    int val = 0;
                    for (String v : vertexes) {
                        if (v.charAt(0) == '!') {
                            mask |= net.getMask(Integer.parseInt(v.substring(1)));
                        } else {
                            int vMask = net.getMask(Integer.parseInt(v));
                            mask |= vMask;
                            val |= vMask;
                        }
                    }
                    double prob = getProb(nextLine.substring(sep + 2));
                    mapping.put(val | net.getMask(u), prob);
                    mapping.put(val, 1 - prob);
                }
                if (!mapping.isEmpty())
                    net.setFactor(u, new Factor(mask, mapping));
            }
            nextLine = reader.readLine();
            String[] fixed = getElements(nextLine);
            int mask = 0;
            int val = 0;
            for (String f : fixed) {
                if (f.charAt(0) == '!') {
                    mask |= net.getMask(Integer.parseInt(f.substring(1)));
                } else {
                    int vMask = net.getMask(Integer.parseInt(f));
                    mask |= vMask;
                    val |= vMask;
                }
            }
            double[] aposteri = net.eliminate(mask, val);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    private static int getSeparator(String s) {
        return s.indexOf(':');
    }

    private static String[] getElements(String s) {
        return s.split(" ");
    }

    private static double getProb(String s) {
        double prob = Double.parseDouble(s);
        if (prob < 0 || prob > 1)
            throw new IllegalStateException("Prob must be > 0 && < 1");
        return prob;
    }
}
