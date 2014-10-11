package ml.bayes;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * @author Arkadii Rost
 */
public class Main {
    private static final float SPLIT_FACTOR = 0.8f;

    private static List<Mail> readMails(File mailRoot) {
        List<Mail> mails = new ArrayList<>();
        try {
            Files.walk(mailRoot.toPath())
                    .filter(path -> !path.toFile().isDirectory())
                    .forEach(mailPath -> {
                        try (Scanner scanner = new Scanner(Files.newBufferedReader(mailPath))) {
                            Map<String, Integer> freq = new HashMap<>();
                            while (scanner.hasNext()) {
                                String word = scanner.next();
                                freq.put(word, freq.getOrDefault(word, 0) + 1);
                            }
                            mails.add(new Mail(freq, isSpam(mailPath)));
                        } catch (IOException e) {
                            throw new RuntimeException("Unexpected IO exception", e);
                        }
                    });
        } catch (IOException e) {
            throw new RuntimeException("Unexpected IO exception", e);
        }
        return mails;
    }

    private static boolean isSpam(Path mailPath) {
        return mailPath.getFileName().toString().contains("spmsg");
    }

    public static void main(String[] args) {
        File mailRoot = new File("bayes");
        List<Mail> mails = readMails(mailRoot);
        Collections.shuffle(mails);
        int splitIndex = (int)(SPLIT_FACTOR * mails.size());
        List<Mail> trainData = mails.subList(0, splitIndex);
        List<Mail> testData = mails.subList(splitIndex, mails.size());
        MailFilter mailFilter = new MailFilter(trainData);
        System.out.printf("Recognized on train data %.2f%%\n", calcPercent(trainData, mailFilter));
        System.out.printf("Recognized on test data %.2f%%\n", calcPercent(testData, mailFilter));
    }

    private static double calcPercent(List<Mail> mails, MailFilter filter) {
        int count = 0;
        for (Mail mail : mails)
            count += filter.isSpam(mail) == mail.isSpam() ? 1 : 0;
        return 100. * count / mails.size();
    }
}
