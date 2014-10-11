package ml.bayes;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Arkadii Rost
 */
public class MailFilter {
    private final Map<String, Double> spamProbs;
    private final Map<String, Double> notSpamProbs;
    private final double pSpam;
    private final double pNotSpam;


    private static Map<String, Double> getProbabilities(List<Mail> msgs, Set<String> words) {
        Map<String, Integer> count = msgs.stream()
                .flatMap(mail -> mail.getFrequences().entrySet().stream())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (x, y) -> x + y));
        for (String word : words)
            count.put(word, count.getOrDefault(word, 0) + 1);
        double totalCount = count.values().stream().mapToInt(Integer::intValue).sum();
        return count.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue() / totalCount));
    }

    public MailFilter(List<Mail> mails) {
        Set<String> words = mails.stream()
                .flatMap(mail -> mail.getFrequences().keySet().stream())
                .collect(Collectors.toSet());
        List<Mail> spamMsgs = mails.stream().filter(Mail::isSpam).collect(Collectors.toList());
        List<Mail> notSpamMsgs = mails.stream().filter(m -> !m.isSpam()).collect(Collectors.toList());
        spamProbs = getProbabilities(spamMsgs, words);
        notSpamProbs = getProbabilities(notSpamMsgs, words);
        pSpam = 1. * spamMsgs.size() / mails.size();
        pNotSpam = 1. * spamMsgs.size() / mails.size();
    }

    private double score(Map<String, Integer> freq, double prior, Map<String, Double> probs) {
        double score = Math.log(prior);
        for (Map.Entry<String, Integer> entry : freq.entrySet())
            score += entry.getValue() * Math.log(probs.getOrDefault(entry.getKey(), 1.0));
        return score;
    }

    private double score(Mail mail,  double prior, Map<String, Double> probs) {
        return score(mail.getFrequences(), prior, probs);
    }

    public boolean isSpam(Mail mail) {
        return score(mail, pSpam, spamProbs) > score(mail, pNotSpam, notSpamProbs);
    }
}
