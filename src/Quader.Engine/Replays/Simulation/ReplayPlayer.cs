namespace Quader.Engine.Replays.Simulation;

public class ReplayPlayer
{
    public Replay? Replay { get; private set; }
    public bool IsLoaded => Replay != null;
    
    public void LoadReplay(Replay replay)
    {
        Replay = replay;
    }

    public void UnloadReplay()
    {
        Replay = null;
    }
}