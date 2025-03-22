namespace Quader.Engine;

public class TimeManager
{
    public float ElapsedSeconds { get; private set; }
    public bool IsEnabled { get; set; } = true;
    public float LastDt { get; private set; }

    public TimeManager()
    {
    }

    public void Update(float dt)
    {
        if (!IsEnabled) return;

        LastDt = dt;
        ElapsedSeconds += dt;
    }

    public void Reset()
    {
        ElapsedSeconds = 0;
        LastDt = 0;
    }

    public void Enable()
    {
        IsEnabled = true;
    }

    public void Disable()
    {
        IsEnabled = false;
    }
}