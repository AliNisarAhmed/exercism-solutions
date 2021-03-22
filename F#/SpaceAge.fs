module SpaceAge

// TODO: define the Planet type

type Planet =
    | Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune



let age (planet: Planet) (seconds: int64) : float =
    let secondsToEarthYears (secs: int64) = (float secs) / 31557600.0

    match planet with
    | Mercury -> secondsToEarthYears seconds / 0.2408467
    | Venus -> secondsToEarthYears seconds / 0.61519726
    | Earth -> secondsToEarthYears seconds
    | Mars -> secondsToEarthYears seconds / 1.8808158
    | Jupiter -> secondsToEarthYears seconds / 11.862615
    | Saturn -> secondsToEarthYears seconds / 29.447498
    | Uranus -> secondsToEarthYears seconds / 84.046846
    | Neptune -> secondsToEarthYears seconds / 164.79132
