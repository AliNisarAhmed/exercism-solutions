using System;

public class SpaceAge
{
    private int _seconds;
    public double EarthSeconds = 31557600;

    public SpaceAge(int seconds)
    {
        _seconds = seconds;
    }

    public double OnEarth()
    {
        return _seconds / EarthSeconds;
    }

    public double OnMercury()
    {
        return _seconds / (0.2408467 * EarthSeconds);
    }

    public double OnVenus()
    {
        return _seconds / (EarthSeconds * 0.61519726);
    }

    public double OnMars()
    {
        return _seconds / (EarthSeconds * 1.8808158);
    }

    public double OnJupiter()
    {
        return _seconds / (EarthSeconds * 11.862615);
    }

    public double OnSaturn()
    {
        return _seconds / (EarthSeconds * 29.447498);
    }

    public double OnUranus()
    {
        return _seconds / (EarthSeconds * 84.016846);
    }

    public double OnNeptune()
    {
        return _seconds / (EarthSeconds * 164.79132);
    }
}