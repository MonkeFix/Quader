namespace Quader.Engine;

public class RngManager
{
    private Random _random;
    public int Seed { get; private set; }

    public Random Random => _random;

    public RngManager()
    {
        Seed = Environment.TickCount;
        _random = new Random(Seed);
    }

    public RngManager(int seed)
    {
        Seed = seed;
        _random = new Random(Seed);
    }

    public void SetSeed(int seed)
    {
        Seed = seed;
        _random = new Random(Seed);
    }
}