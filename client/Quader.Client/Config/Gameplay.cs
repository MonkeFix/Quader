using Nez.Persistence;

namespace Quader.Config;

public class Gameplay : IGameConfigEntry<Gameplay>
{
    [JsonInclude]
    public float GridVisibility { get; set; }
    [JsonInclude]
    public float GhostVisibility { get; set; }
    [JsonInclude]
    public float BoardZoom { get; set; }

    public Gameplay Defaults()
    {
        return new Gameplay
        {
            GridVisibility = 0.1f,
            GhostVisibility = 0.9f,
            BoardZoom = 1.0f
        };
    }
}