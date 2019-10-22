using System;
using System.Linq;

public static class RotationalCipher
{
    public static string Rotate(string text, int shiftKey)
    {
        return string.Concat(text.Select(c =>
        {
            if (char.IsLetter(c))
            {
                return char.IsUpper(c)
                    ? Convert.ToChar(AddShiftAndNormalize(c, shiftKey, 65))
                    : Convert.ToChar(AddShiftAndNormalize(c, shiftKey, 97));
            }
            return c;
        }));
    }

    private static int AddShiftAndNormalize(char c, int shift, int code)
    {
        return ((Convert.ToInt32(c) + shift - code) % 26) + code;
    }
}