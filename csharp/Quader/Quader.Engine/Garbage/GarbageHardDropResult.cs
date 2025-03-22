namespace Quader.Engine.Garbage;

public struct GarbageHardDropResult
{
    public List<IncomingDamage> InDamageQueue;
    public int OutDamage;

    public GarbageHardDropResult()
    {
        InDamageQueue = new List<IncomingDamage>();
        OutDamage = 0;
    }

    public GarbageHardDropResult(List<IncomingDamage> inDamageQueue, int outDamage)
    {
        InDamageQueue = inDamageQueue;
        OutDamage = outDamage;
    }
}