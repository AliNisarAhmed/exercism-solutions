import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.TemporalUnit;
import java.util.Optional;

public class Gigasecond {
    final long gigaSeconds = 1_000_000_000;
    private LocalDateTime dateTime;

    public Gigasecond(LocalDate moment) {
        this(moment.atStartOfDay());
    }

    public Gigasecond(LocalDateTime moment) {
        this.dateTime = moment;
    }

    public LocalDateTime getDateTime() {
        return this.dateTime.plusSeconds(this.gigaSeconds);
    }
}
