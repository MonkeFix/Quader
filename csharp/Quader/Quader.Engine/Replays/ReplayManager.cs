namespace Quader.Engine.Replays;

public class ReplayManager
{
    public List<ReplayMove> Moves { get; }
    public List<ReplayMove> CurrentMoveQueue { get; }

    public ReplayManager()
    {
        Moves = new List<ReplayMove>();
        CurrentMoveQueue = new List<ReplayMove>();
    }

    public void AddMove(float timestamp, MoveAction action)
    {
        var move = new ReplayMove(action, timestamp);
        Moves.Add(move);
        CurrentMoveQueue.Add(move);
    }

    public ReplayMove[] EndMove()
    {
        var q = CurrentMoveQueue.ToArray();
        CurrentMoveQueue.Clear();
        return q;
    }

    public void Reset()
    {
        Moves.Clear();
        CurrentMoveQueue.Clear();
    }
}