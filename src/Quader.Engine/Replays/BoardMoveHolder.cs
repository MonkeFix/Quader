using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Nez.Persistence;
using Nez.Persistence.Binary;

namespace Quader.Engine.Replays;

public class BoardMoveHolder : IPersistable
{
    [JsonExclude]
    public Board Board { get; }
    [JsonInclude]
    public List<BoardMove> Moves { get; private set; }
    [JsonInclude]
    public DateTimeOffset StartDate { get; private set; }
    [JsonInclude]
    public DateTimeOffset EndDate { get; private set; }

    [JsonExclude]
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

    public void Recover(IPersistableReader reader)
    {
        // TODO: Save Game Settings!!!
        StartDate = DateTime.Parse(reader.ReadString());
        EndDate = DateTime.Parse(reader.ReadString());
        var count = reader.ReadInt();
        var list = new List<BoardMove>();
        for (int i = 0; i < count; i++)
        {
            var tmp = new BoardMove();
            reader.ReadPersistableInto(tmp);
            list.Add(tmp);
        }

        Moves = list;
    }

    public void Persist(IPersistableWriter writer)
    {
        writer.Write(StartDate.ToString(CultureInfo.InvariantCulture));
        writer.Write(EndDate.ToString(CultureInfo.InvariantCulture));
        writer.Write(Moves.Count);
        foreach (var m in Moves)
        {
            writer.Write(m);
        }
    }
}