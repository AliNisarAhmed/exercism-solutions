import java.util.stream.IntStream;

public class Hamming {
    private int hammingDistance;

    public Hamming(String leftStrand, String rightStrand) {
        this.hammingDistance = this.calculateHammingDistance(leftStrand, rightStrand);
    }

    private int calculateHammingDistance(String leftStrand, String rightStrand) {
        int leftStrandLength = leftStrand.length();
        int rightStrandLength = rightStrand.length();

        if (leftStrandLength != rightStrandLength && leftStrandLength == 0) {
            throw new IllegalArgumentException("left strand must not be empty.");
        }

        if (leftStrandLength != rightStrandLength && rightStrandLength == 0) {
            throw new IllegalArgumentException("right strand must not be empty.");
        }

        if (leftStrand.length() != rightStrand.length()) {
            throw new IllegalArgumentException("leftStrand and rightStrand must be of equal length.");
        }

        return (int) IntStream
                .range(0, leftStrandLength)
                .filter(i -> leftStrand.charAt(i) != rightStrand.charAt(i))
                .count();
    }

    public int getHammingDistance() {
        return this.hammingDistance;
    }
}
