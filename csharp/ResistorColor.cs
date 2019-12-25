using System;

public static class ResistorColor
{
    public static int ColorCode(string color)
    {
        return Array.FindIndex(Colors(), c => c == color);
    }

    public static string[] Colors()
    {
        return new string[]
        {
            "black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"
        };
    }
}