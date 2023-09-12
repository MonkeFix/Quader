using System;

namespace Quader.Engine;

public static class RandomMgr
{
    public static Random Random { get; private set; }

    static RandomMgr()
    {
        Random = new Random();
    }

    public static void SetSeed(int seed)
    {
        Random = new Random(seed);
    }
}