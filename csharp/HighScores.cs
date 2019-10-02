using System;
using System.Collections.Generic;
using System.Linq;

public class HighScores
{
    public List<int> ListOfScores { get; }

    public HighScores(List<int> list)
    {
        ListOfScores = list;
    }

    public List<int> Scores()
    {
        return ListOfScores;
    }

    public int Latest() => ListOfScores.Last();

    public int PersonalBest()
    {
        return ListOfScores.Max();
    }

    public List<int> PersonalTopThree() => ListOfScores.OrderByDescending(x => x).Take(3).ToList();
}