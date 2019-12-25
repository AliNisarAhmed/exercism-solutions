using System;
using System.Linq;
using System.Collections.Generic;

public static class RnaTranscription
{
    public static string ToRna(string nucleotide)
    {
        var h = new Dictionary<Char, Char>();
        h.Add('G', 'C');
        h.Add('C', 'G');
        h.Add('T', 'A');
        h.Add('A', 'U');

        return string.Concat(nucleotide.Select(x => h[x]));
    }
}