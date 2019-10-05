using System.Collections.Generic;
using System.Linq;

public static class Isogram
{
    public static bool IsIsogram(string word)
    {
        var wordsOnly = word.Where(char.IsLetter);

        return string.Concat(
            wordsOnly
              .Select(char.ToLower)
              .Distinct()
            ).Length == string.Concat(wordsOnly).Length;
    }
}
