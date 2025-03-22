namespace Quader.Engine.Garbage;

public struct IncomingDamage
{
    public int Amount;
    public float Delay;
    public uint HoleX;

    public IncomingDamage()
    {
    }

    public IncomingDamage(int amount, float delay, uint holeX)
    {
        Amount = amount;
        Delay = delay;
        HoleX = holeX;
    }

    public void Update(TimeManager timeManager)
    {
        Delay -= timeManager.ElapsedSeconds;
    }
}