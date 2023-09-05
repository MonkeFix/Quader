using System;
using System.Collections.Generic;

namespace Quader.Engine.Replays;

public class BoardMoveHolder
{
    public Board Board { get; }
    public List<BoardMove> Moves { get; private set; }
    public DateTimeOffset StartDate { get; private set; }
    public DateTimeOffset EndDate { get; private set; }
    public bool HasEnded { get; private set; }

    public BoardMoveHolder(Board board, DateTimeOffset startDate)
    {
        Board = board;
        Moves = new List<BoardMove>(1000);
        StartDate = startDate;
    }

    public BoardMoveHolder AddMove(BoardHardDropInfo? move, double tick, BoardMoveType type, ReplayMoveInfo info = default)
    {
        if (HasEnded)
            throw new Exception("BoardMoveHolder has already ended");

        if (type == BoardMoveType.Idle)
            return this;

        Moves.Add(new BoardMove
        {
            Tick = tick,
            HardDropInfo = move,
            Type = type,
            Info = info
        });

        return this;
    }

    public BoardMoveHolder End(DateTimeOffset endTime)
    {
        EndDate = endTime;
        HasEnded = true;
        return this;
    }
}