using System;

public static class BeerSong
{
    public static string Recite(int startBottles, int takeDown)
    {
        var result = "";
        while (takeDown > 0)
        {
            result = result + ReturnVerse(startBottles, takeDown);
            startBottles--;
            takeDown--;
            if (takeDown > 0)
                result += "\n\n";
        }
        return result;
    }

    public static string ReturnVerse(int first, int second)
    {
        if (first == 0)
        {
            return "No more bottles of beer on the wall, no more bottles of beer.\n" +
            "Go to the store and buy some more, 99 bottles of beer on the wall.";
        }
        else
        {
            return $"{first} {SingularOrPlural(first)} of beer on the wall, {first} {SingularOrPlural(first)} of beer.\n" +
            $"Take {(first == 1 ? "it" : "one")} down and pass it around, {NumberOrNoMore(first - 1)} {SingularOrPlural(first - 1)} of beer on the wall.";
        }
    }

    public static string SingularOrPlural(int v) => v == 1 ? "bottle" : "bottles";

    public static string NumberOrNoMore(int v) => v == 0 ? "no more" : v.ToString();

}