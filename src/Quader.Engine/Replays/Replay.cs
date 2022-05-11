using System.Collections.Generic;
using Quader.Engine.Replays;

namespace Quader.Engine;

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