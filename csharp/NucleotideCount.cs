using System;
using System.Collections.Generic;

public static class NucleotideCount
{
    public static IDictionary<char, int> Count(string sequence)
    {
        var validNucleotides = "ACGT";
        var dict = new Dictionary<char, int>();
        
        foreach (var v in validNucleotides)
        {
            dict.Add(v, 0);
        }

        foreach (var c in sequence)
        {
            if (validNucleotides.Contains(c)) 
            {
                dict[c] += 1;
            }
            else
            {
                throw new ArgumentException();
            }
        }

        return dict;
    }
}