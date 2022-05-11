using System.Collections.Generic;

namespace Quader.Engine.Replays;

public class Replay
{
    public List<BoardMove> Moves { get; }

    public Replay()
    {
        Moves = new List<BoardMove>();
    }

    public void AddMove(BoardMove move)
    {
        Moves.Add(move);
    }
}