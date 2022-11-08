using System;
using System.Collections.Generic;
using System.Linq;
using Quader.Engine.Settings;

namespace Quader.Engine.Replays;

public class Replay
{
    public IEnumerable<BoardMoveHolder> Boards { get; }
    public GameSettings GameSettings { get; }
    
    public DateTimeOffset StartDate { get; }
    public DateTimeOffset EndDate { get; }

    private Replay(IEnumerable<BoardMoveHolder> boards, GameSettings gameSettings)
    {
        Boards = boards.ToList();
        
        if (!Boards.Any())
            throw new Exception("Board list is empty");

        GameSettings = gameSettings;

        var first = Boards.First();
        StartDate = first.StartDate;
        EndDate = first.EndDate;
    }

    public static Replay Create(IEnumerable<BoardMoveHolder> boards, GameSettings gameSettings)
    {
        return new Replay(boards, gameSettings);
    }
}