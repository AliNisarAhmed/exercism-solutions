using System;
using System.Collections.Generic;

public class Robot
{
    private static List<string> usedNames = new List<string>();

    public Robot()
    {
        var name = getName();
        while(true)
        {
            if (usedNames.Contains(name))
            {
                name = getName();
            }
            else
            {
                usedNames.Add(name);
                Name = name;
                break;
            }
        }

    }

    public string getName()
    {
        var rnd = new Random();
        var numString = rnd.Next(0, 1000).ToString();
        var alphabets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var first = alphabets[rnd.Next(0, 26)];
        var second = alphabets[rnd.Next(0, 26)];
        return first.ToString() + second.ToString() + numString;
    }

    public string Name { get; private set; }

    public void Reset()
    {
        usedNames = new List<string>();
        this.Name = getName();
    }
}