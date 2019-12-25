using System;
using System.Collections.Generic;

public static class Raindrops
{
    public static string Convert(int number)
    {
        var result = string.Concat(new List<string>()
        {
            number % 3 == 0 ? "Pling" : "", 
            number % 5 == 0 ? "Plang" : "",
            number % 7 == 0 ? "Plong" : "",
        });

        return result.Length == 0 ? number.ToString() : result;
    }
}