using System;
using System.Collections.Generic;
using System.Linq;

public enum Classification
{
    Perfect,
    Abundant,
    Deficient
}

public static class PerfectNumbers
{
    public static Classification Classify(int number)
    {
        if (number <= 0)
        {
            throw new ArgumentOutOfRangeException();
        }
        var sum = Factors(number).Sum();
        if (sum == number)
        {
            return Classification.Perfect;
        }
        if (sum > number)
        {
            return Classification.Abundant;
        }
        else
        {
            return Classification.Deficient;
        }
    }

    public static List<int> Factors(int m) => Enumerable.Range(1, m / 2).Where(n => m % n == 0).ToList();
}
