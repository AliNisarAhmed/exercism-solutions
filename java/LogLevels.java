import java.util.Locale;

public class LogLevels {
    
    public static String message(String logLine) {
        return logLine.split(":")[1].trim();
    }

    public static String logLevel(String logLine) {
        return logLine
                .split(":")[0]
                .replaceAll("[\\[\\]]", "")
                .toLowerCase();
    }

    public static String reformat(String logLine) {
        Object replacements[] = { message(logLine), logLevel(logLine)};
        return "%s (%s)".formatted(replacements);
    }
}
