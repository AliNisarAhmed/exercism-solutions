using System;
using System.Linq;

public static class Grains
{
    public static ulong Square(int n)
    {
        if (n <= 0 || n > 64)
        {
            throw new ArgumentOutOfRangeException("Out of range");
        }
        else
        {
            return (ulong)Math.Pow(2, n - 1);
        }
    }

    public static ulong Total() => 
        Enumerable
            .Range(1, 64)
            .Select(x => Square(x))
            .Aggregate((acc, x) => acc + x);
}