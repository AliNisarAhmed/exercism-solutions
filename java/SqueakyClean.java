class SqueakyClean {

    static String clean(String identifier) {
        StringBuilder stb = new StringBuilder();
        boolean toBeConvertedToKebab = false;

        for (char current : identifier.toCharArray()) {
            if (Character.isWhitespace(current)) {
                stb.append("_");
            } else if (Character.isISOControl(current)) {
                stb.append("CTRL");
            } else if (current == '-') {
                toBeConvertedToKebab = true;
            } else if (toBeConvertedToKebab) {
                toBeConvertedToKebab = false;
                stb.append(Character.toUpperCase(current));
            } else if (Character.isLetter(current) && !isGreekLetter(current)) {
                stb.append(current);
            }
        }

        return stb.toString();
    }

    static boolean isGreekLetter(char c) {
        return c >= '\u03b1' && c <= '\u03c9';
    }
}
