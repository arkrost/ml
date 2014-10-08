package ml;

import ml.associative.Apriori;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author Arkadii Rost
 */
public class Main {
    private static final String DATA_PATH = "supermarket.arff";

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader(DATA_PATH))) {
            br.readLine(); // @relation
            br.readLine(); //
            br.readLine(); // @attribute product_name

            //parse @attribute product_category
            Map<String, Integer> attributeMap = new HashMap<>();

            String attrs = br.readLine();
            attrs = attrs.substring("@attribute product_category {".length(), attrs.length() - 1);
            int attrId = 0;
            for (String productCategory : attrs.split(","))
                attributeMap.put(productCategory, attrId++);

            br.readLine(); // @attribute product_department
            br.readLine(); // @attribute basket_id
            br.readLine(); //
            br.readLine(); // @data

            //parse baskets
            String entry;
            Map<Long, Set<Integer>> basketsMap = new HashMap<>();
            while ((entry = br.readLine()) != null) {
                String productCategory = entry.split(",")[1];
                Long basketId = Long.valueOf(entry.substring(entry.lastIndexOf(',') + 1));
                if (!basketsMap.containsKey(basketId))
                    basketsMap.put(basketId, new HashSet<Integer>());
                basketsMap.get(basketId).add(attributeMap.get(productCategory));
            }
            boolean[][] baskets = new boolean[basketsMap.size()][attributeMap.size()];
            int i = 0;
            for (Set<Integer> basket : basketsMap.values()) {
                for (int id : basket)
                   baskets[i][id] = true;
                i++;
            }
            Apriori apriori = new Apriori(attributeMap, baskets);
            apriori.init();
            for (Apriori.Rule rule : apriori.getRules())
                System.out.println(rule);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
