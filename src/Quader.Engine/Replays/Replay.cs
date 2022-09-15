using System.Collections.Generic;
using Nez.Persistence;

namespace Quader.Engine.Replays;

public class Replay
{
    [JsonExclude]
    public Board Board { get; }
    [JsonInclude]
    public List<ReplayMove> Moves { get; }
    [JsonInclude]
    public double StartTick { get; }

    public Replay(Board board, long startTick)
    {
        Board = board;
        Moves = new List<ReplayMove>(1_000_000);
        StartTick = startTick;
    }

    public void AddMove(BoardMove? move, double tick, ReplayMoveType type, ReplayMoveInfo info = default)
    {
        if (type == ReplayMoveType.Idle)
            return;

        Moves.Add(new ReplayMove
        {
            Tick = tick,
            BoardMove = move,
            Type = type,
            Info = info
        });
    }
}