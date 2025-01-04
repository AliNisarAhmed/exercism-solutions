// Package weather forecasts the current weather conditions
// of various cities in Goblinocus.
package weather

// CurrentCondition describes current weather conditions in a city.
var CurrentCondition string
// CurrentLocation describes the location of the city in question.
var CurrentLocation string

// Forecast returns the formatted location and weather condition of a city.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " + CurrentCondition
}
