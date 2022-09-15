namespace Quader.Managers.TimeProviders;

public interface ITimeProvider
{
    double CurrentTimeMs { get; }
    ulong UpdateCycles { get; }
    float DeltaTime { get; }

    void Update();
}