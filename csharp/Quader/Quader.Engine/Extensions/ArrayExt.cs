namespace Quader.Engine.Extensions;

public static class ArrayExt
{
    public static void Shuffle<T>(this T[] list, Random random)
    {
        var n = list.Length;
        while (n > 1)
        {
            n--;
            int k = random.Next(0, n + 1);
            (list[k], list[n]) = (list[n], list[k]);
        }
    }
}