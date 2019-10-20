using System;
using System.Collections.Generic;
using System.Linq;

public class PhoneNumber
{
    private static readonly string validNumbers = "23456789";
    public static string Clean(string phoneNumber)
    {
        var filtered = phoneNumber.Where(char.IsNumber);

        var count = filtered.Count();

        if (count < 10 || count > 11)
        {
            throw new ArgumentException();
        }

        if (count == 11)
        {
            if (filtered.First() != '1')
            {
                throw new ArgumentException();
            }
        }

        filtered = filtered.TakeLast(10);

        if (!CheckValidNumber(filtered))
        {
            throw new ArgumentException();
        }

        return string.Concat(filtered);
    }

    private static bool CheckValidNumber(IEnumerable<char> number)
    {
        return validNumbers.Contains(number.ElementAt(0)) && validNumbers.Contains(number.ElementAt(3)); 
    }
}