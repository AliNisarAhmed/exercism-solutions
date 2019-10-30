using System;

public class Clock : IEquatable<Clock>
{
    public int Hours { get; }
    public int Minutes { get; }

    public Clock(int hours, int minutes)
    {
        var (divMinutes, remMinutes) = DivRemMinutes(minutes);
        Hours = ((hours + divMinutes) % 24 + 24) % 24;
        Minutes = remMinutes;
    }

    public override string ToString()
    {

        return $"{Hours.ToString().PadLeft(2, '0')}:{Minutes.ToString().PadLeft(2, '0')}";
    }

    public Clock Add(int minutesToAdd)
    {
        var div = Math.DivRem(minutesToAdd, 60, out int rem);
        return new Clock(Hours + div, Minutes + rem);
    }

    public Clock Subtract(int minutesToSubtract)
    {
        var div = Math.DivRem(minutesToSubtract, 60, out int rem);
        return new Clock(Hours - div, Minutes - rem);
    }

    private (int, int) DivRemMinutes(int minutes)
    {
        var div = Math.Floor((decimal)minutes / 60);
        var rem = ((minutes % 60) + 60) % 60;
        return ((int)div, rem);
    }

    public bool Equals(Clock other) => Hours == other.Hours && Minutes == other.Minutes;
}

