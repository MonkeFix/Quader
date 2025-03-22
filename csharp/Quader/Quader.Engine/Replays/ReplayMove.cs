namespace Quader.Engine.Replays;

public struct ReplayMove
{
    public MoveAction Action;
    public float Timestamp;

    public ReplayMove(MoveAction action, float timestamp)
    {
        Action = action;
        Timestamp = timestamp;
    }
}