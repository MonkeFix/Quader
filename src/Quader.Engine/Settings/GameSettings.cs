using Nez.Persistence;

namespace Quader.Engine.Settings;

public class GameSettings
{
    private static GameSettings? _default;

    public static GameSettings Default
    {
        get
        {
            if (_default == null)
                _default = new GameSettings
                {
                    Board = new BoardSettings
                    {
                        BoardWidth = 10,
                        BoardHeight = 20,
                    },
                    Gravity = new GravitySettings()
                };

            return _default;
        }
    }
    [JsonInclude]
    public GravitySettings Gravity { get; set; } = null!;
    [JsonInclude]
    public BoardSettings Board { get; set; } = null!;
}