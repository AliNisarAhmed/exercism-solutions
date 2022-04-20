import java.util.HashMap;

class Scrabble {

    final private String word;
    private static final HashMap<Integer, Integer> scores = new HashMap<>(26);

    static {
        "AEIOULNRST".chars().forEach(c -> scores.put(c, 1));
        "DG".chars().forEach(c -> scores.put(c, 2));
        "BCMP".chars().forEach(c -> scores.put(c, 3));
        "FHVWY".chars().forEach(c -> scores.put(c, 4));
        "K".chars().forEach(c -> scores.put(c, 5));
        "JX".chars().forEach(c -> scores.put(c, 8));
        "QZ".chars().forEach(c -> scores.put(c, 10));
    }

    Scrabble(String word) {
        this.word = word.toUpperCase();
    }

    int getScore() {
        return this.word.chars().reduce(0, (acc, x) -> acc + scores.get(x));
    }
}
