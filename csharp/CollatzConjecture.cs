using System;

public static class CollatzConjecture
{
    public static int Steps(int number)
    {
        if (number <= 0)
        {
            throw new ArgumentException();
        }

        return Collatz(number, 0);
    }

    public static int NextCollatzStep(int n) => n % 2 == 0 ? (n / 2) : (3 * n + 1);

    public static int Collatz(int m, int count) => m == 1 ? count : Collatz(NextCollatzStep(m), count + 1);
}