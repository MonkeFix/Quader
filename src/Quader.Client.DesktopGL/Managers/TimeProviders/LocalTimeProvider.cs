using Nez;

namespace Quader.Managers.TimeProviders;

public class LocalTimeProvider : ITimeProvider
{
    public double CurrentTimeMs { get; private set; }
    public ulong UpdateCycles { get; private set; }
    public float DeltaTime { get; private set; }

    public LocalTimeProvider()
    {
        CurrentTimeMs = 0;
        UpdateCycles = 0;
    }

    public void Update()
    {
        DeltaTime = Time.DeltaTime;
        CurrentTimeMs += DeltaTime * 1000;
        UpdateCycles++;
    }
}