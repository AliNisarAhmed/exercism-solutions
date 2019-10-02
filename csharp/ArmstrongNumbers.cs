using System;
using System.Linq;

public static class ArmstrongNumbers
{
    public static bool IsArmstrongNumber(int number)
    {
        var numberString = number.ToString();
        var numOfDigits = numberString.Length;
        return numberString
                 .Select(x => Math.Pow(int.Parse(x.ToString()), numOfDigits))
                 .Sum()
                 .Equals(number);
    }
}