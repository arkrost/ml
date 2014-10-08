package ml.associative;

import java.util.*;

/**
 * @author Arkadii Rost
 */
public class Apriori {
    private static final double SUPPORT = 0.1;
    private static final double CONFIDENCE = 0.1;

    private final Map<String, Integer> attributeMap;
    private final Map<Integer, String> idsMap;
    private final boolean[][] baskets;

    private final int minSupport;

    private List<List<Basket>> generations;
    private Set<Rule> rules;

    public Apriori(Map<String, Integer> attributeMap, boolean[][] baskets) {
        this.attributeMap = attributeMap;
        this.baskets = baskets;
        this.idsMap = new HashMap<>();
        for (Map.Entry<String, Integer> entry : attributeMap.entrySet())
            idsMap.put(entry.getValue(), entry.getKey());
        this.minSupport = (int)(SUPPORT * baskets.length);
    }

    public void init() {
        generations = new ArrayList<>();
        System.out.println("Calculating 1 generation.");
        generations.add(generateF1());
        while (generations.size() < attributeMap.size()) {
            int k = generations.size();
            System.out.println("Calculating " + (k + 1) +" generation.");
            List<Basket> nextGeneration = generateFNext(generations.get(k - 1), k);
            if (nextGeneration.isEmpty()) {
                System.out.println((k+1) + " generation is empty.");
                break;
            }
            generations.add(nextGeneration);
        }
        extractRules();
    }

    public Set<Rule> getRules() {
        return rules;
    }

    private void extractRules() {
        System.out.println("Extracting rules.");
        rules = new HashSet<>();
        ListIterator<List<Basket>> iterator = generations.listIterator(1);
        while (iterator.hasNext()) {
            for (Basket b : iterator.next())
                extractRule(b);
        }
    }

    private void extractRule(Basket basket) {
        Set<Integer> productsSet = new TreeSet<>();
        productsSet.addAll(basket.getProducts());
        double freq = 1. * getFreq(basket.getProducts());
        for (Set<Integer> from : generateAllSubsets(productsSet)) {
            if (from.isEmpty() || from.size() == productsSet.size())
                continue;
            List<Integer> fromList = new ArrayList<>(from.size());
            fromList.addAll(from);
            List<Integer> toList = new ArrayList<>();
            toList.addAll(basket.getProducts());
            toList.removeAll(fromList);
            double confidence = freq /  getFreq(fromList);
            if (confidence > CONFIDENCE)
                rules.add(new Rule(fromList, toList, confidence));
        }
    }

    private List<Basket> generateF1() {
        List<List<Integer>> singles = new ArrayList<>();
        for (int id : attributeMap.values())
           singles.add(Collections.singletonList(id));
        return generateBasketList(singles);
    }

    private List<Basket> generateFNext(List<Basket> pred, int k) {
        Set<List<Integer>> candidates = generateCandiates(pred, k);
        candidates = filterCandidates(candidates, toProductSet(pred), k);
        return generateBasketList(candidates);
    }

    private List<Basket> generateBasketList(Collection<List<Integer>> candidates) {
        List<Basket> res = new ArrayList<>();
        for (List<Integer> candidate : candidates) {
            int freq = countFreq(candidate);
            if (!(freq < minSupport))
                res.add(new Basket(candidate, freq));
        }
        return res;
    }

    private Set<List<Integer>> toProductSet(List<Basket> baskets) {
        Set<List<Integer>> res = new HashSet<>();
        for (Basket b : baskets)
            res.add(b.getProducts());
        return res;
    }

    private Set<List<Integer>> generateCandiates(List<Basket> pred, int k) {
        Set<List<Integer>> candidates = new HashSet<>();
        for (int i = 0; i < pred.size(); i++) {
            Basket basketA = pred.get(i);
            List<Integer> sublist = getSublist(basketA, k - 1);
            int lastA = basketA.getProducts().get(k - 1);
            for (int j = i + 1; j < pred.size(); j++) {
                Basket basketB = pred.get(j);
                if (sublist.equals(getSublist(basketB, k - 1))) {
                    int lastB = basketB.getProducts().get(k - 1);
                    List<Integer> merged = new ArrayList<>(k);
                    merged.addAll(sublist);
                    if (lastA > lastB) {
                        merged.add(lastB);
                        merged.add(lastA);
                    } else {
                        merged.add(lastA);
                        merged.add(lastB);
                    }
                    candidates.add(merged);
                }
            }
        }
        return candidates;
    }

    private Set<List<Integer>> filterCandidates(Set<List<Integer>> next, Set<List<Integer>> pred, int k) {
        Set<List<Integer>> maybeGoodEnough = new HashSet<>();
        B : for (List<Integer> products : next) {
            for (int i = 0; i < k - 1; i++) {
                if (!pred.contains(getMinorList(products, i)))
                    continue B;
            }
            maybeGoodEnough.add(products);
        }
        return maybeGoodEnough;
    }

    private List<Integer> getMinorList(List<Integer> products, int i) {
        List<Integer> minorList = new ArrayList<>();
        minorList.addAll(products.subList(0, i));
        minorList.addAll(products.subList(i + 1, products.size()));
        return minorList;
    }

    private static List<Integer> getSublist(Basket b, int k) {
        return b.getProducts().subList(0, k);
    }

    private int countFreq(List<Integer> products) {
        int freq = 0;
        B: for (boolean[] basket : baskets) {
            for (int product : products) {
                if (!basket[product])
                    continue B;
            }
            freq++;
        }
        return freq;
    }

    private int getFreq(List<Integer> products) {
        for (Basket b : generations.get(products.size() - 1)) {
            if (b.getProducts().equals(products))
                return b.getFreq();
        }
        return 0;
    }

    private static <T> Set<Set<T>> generateAllSubsets(Set<T> original) {
        Set<Set<T>> allSubsets = new HashSet<>();

        allSubsets.add(new HashSet<T>()); //Add empty set.

        for (T element : original) {
            Set<Set<T>> tempClone = new HashSet<>(allSubsets);

            for (Set<T> subset : tempClone) {
                Set<T> extended = new HashSet<>(subset);
                extended.add(element);
                allSubsets.add(extended);
            }
        }

        return allSubsets;
    }

    public class Rule {
        private final List<Integer> from;
        private final List<Integer> to;

        private final double confidence;

        private Rule(List<Integer> from, List<Integer> to, double confidence) {
            this.from = from;
            this.to = to;
            this.confidence = confidence;
        }

        public List<Integer> getFrom() {
            return from;
        }

        public List<Integer> getTo() {
            return to;
        }

        public double getConfidence() {
            return confidence;
        }

        private String showProductList(List<Integer> productList) {
            StringBuilder sb = new StringBuilder();
            for (Integer id : productList) {
                sb.append(idsMap.get(id)).append(" ");
            }
            return sb.toString();
        }


        @Override
        public String toString() {
            return  showProductList(from) + "=> " + showProductList(to) + "(with confidence " + confidence + ")";
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Rule rule = (Rule) o;

            if (Double.compare(rule.confidence, confidence) != 0) return false;
            if (!from.equals(rule.from)) return false;
            if (!to.equals(rule.to)) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result;
            long temp;
            result = from.hashCode();
            result = 31 * result + to.hashCode();
            temp = Double.doubleToLongBits(confidence);
            result = 31 * result + (int) (temp ^ (temp >>> 32));
            return result;
        }
    }

    private static class Basket {
        final List<Integer> products;
        final int freq;

        private Basket(List<Integer> products, int freq) {
            this.products = products;
            this.freq = freq;
        }

        public List<Integer> getProducts() {
            return products;
        }

        public int getFreq() {
            return freq;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o)
                return true;
            if (o == null || getClass() != o.getClass())
                return false;
            Basket basket = (Basket) o;
            return freq == basket.freq && products.equals(basket.products);

        }

        @Override
        public int hashCode() {
            int result = products.hashCode();
            result = 31 * result + freq;
            return result;
        }
    }
}
