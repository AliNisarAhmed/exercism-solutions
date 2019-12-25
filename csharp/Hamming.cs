using System;
using System.Linq;

public static class Hamming
{
    public static int Distance(string firstStrand, string secondStrand)
    {
        if (firstStrand.Length != secondStrand.Length)
        {
            throw new ArgumentException("Strings should be of equal length");
        }

        return firstStrand.Where((c, i) => secondStrand[i] != c).Count();
    }
}