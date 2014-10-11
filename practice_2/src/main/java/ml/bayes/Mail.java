package ml.bayes;

import java.util.Map;

/**
 * @author Arkadii Rost
 */
public class Mail {
    private final Map<String, Integer> frequences;
    private final boolean spam;

    public Mail(Map<String, Integer> frequences, boolean spam) {
        this.frequences = frequences;
        this.spam = spam;
    }

    public Map<String, Integer> getFrequences() {
        return frequences;
    }

    public boolean isSpam() {
        return spam;
    }
}
