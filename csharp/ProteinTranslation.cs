using System;
using System.Collections.Generic;
using System.Linq;

public static class ProteinTranslation
{
    private static readonly Dictionary<string, string> Store = new Dictionary<string, string>()
    {
        { "AUG", "Methionine" },
        { "UUU", "Phenylalanine" },
        { "UUC", "Phenylalanine" },
        { "UUA", "Leucine" },
        { "UUG", "Leucine" },
        { "UCU", "Serine" },
        { "UCC", "Serine" },
        { "UCA", "Serine" },
        { "UCG", "Serine" },
        { "UAU", "Tyrosine" },
        { "UAC", "Tyrosine" },
        { "UGU", "Cysteine" },
        { "UGC", "Cysteine" },
        { "UGG", "Tryptophan" },
        { "UAA", "STOP" },
        { "UAG", "STOP" },
        { "UGA", "STOP" },
    };
    public static IEnumerable<string> Proteins(string strand)
    {
        return Enumerable
                .Range(0, strand.Length / 3)
                .Select(i => strand.Substring(i * 3, 3))
                .Select(code => Store[code])
                .TakeWhile(c => c != "STOP");
    }
}