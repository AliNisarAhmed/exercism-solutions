using System;
using System.Linq;

public static class ReverseString
{
    public static string Reverse(string input)
    {
        //return string.Concat(input.Reverse());

        // OR 

        var charArray = input.ToCharArray();
        Array.Reverse(charArray);
        return new string(charArray);
    }
}