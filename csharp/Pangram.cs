using System.Linq;

public static class Pangram
{
    public static bool IsPangram(string input) => 
        string
            .Concat(
                input
                  .Where(s => char.IsLetter(s))
                  .Select(s => char.ToLower(s))
                  .Distinct()
            ).Length == 26;
}
