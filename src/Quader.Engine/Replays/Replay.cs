using System.Collections.Generic;
using System.Linq;
using Quader.Engine.Settings;

namespace Quader.Engine.Replays;

public class Replay
{
    public IEnumerable<Board> Boards { get; }
    public GameSettings GameSettings { get; }

    private Replay(IEnumerable<Board> boards, GameSettings gameSettings)
    {
        Boards = boards;
        GameSettings = gameSettings;
    }

    public static Replay Create(IEnumerable<Board> boards, GameSettings gameSettings)
    {
        return new Replay(boards, gameSettings);
    }
}