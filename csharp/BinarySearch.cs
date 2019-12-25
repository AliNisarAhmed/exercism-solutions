using System;
using System.Linq;

public static class BinarySearch
{
    public static int Find(int[] input, int value)
    {
        var left = 0;
        var right = input.Length - 1;
        while (left <= right)
        {
            int mid = (left + right) / 2;
            if (input[mid] == value)
            {
                return mid;
            }
            else if (value > input[mid])
            {
                left = mid + 1;
            }
            else
            {
                right = mid - 1;
            }
        }
        return -1;
    }
}