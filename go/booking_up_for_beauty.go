package booking

import (
	"fmt"
	"time"
)

// Schedule returns a time.Time from a string containing a date.
func Schedule(date string) time.Time {
	t, e := time.Parse("1/2/2006 15:04:05", date)
	fmt.Println("error: ", e)
	return t
}

// HasPassed returns whether a date has passed.
func HasPassed(date string) bool {
	return time.Now().After(Schedule(date))
}

// IsAfternoonAppointment returns whether a time is in the afternoon.
func IsAfternoonAppointment(date string) bool {
	hour := Schedule(date).Hour()
	return hour >= 12 && hour < 18
}

// Description returns a formatted string of the appointment time.
func Description(date string) string {
	dateParsed := Schedule(date)
	return fmt.Sprintf(
		"You have an appointment on %v, %v %v, %v, at %0d:%0d",
		dateParsed.Weekday(),
		dateParsed.Month(),
		dateParsed.Day(),
		dateParsed.Year(),
		dateParsed.Hour(),
		dateParsed.Minute(),
	)
}

// AnniversaryDate returns a Time with this year's anniversary.
func AnniversaryDate() time.Time {
	return time.Date(2020, time.September, 15, 0, 0, 0, 0, time.UTC)
}
