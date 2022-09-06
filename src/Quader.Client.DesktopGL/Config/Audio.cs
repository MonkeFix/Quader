using Nez.Persistence;

namespace Quader.Config;

public class Audio : IGameConfigEntry<Audio>
{
    [JsonInclude]
    public float Music { get; set; }
    [JsonInclude]
    public float SoundEffects { get; set; }


    public Audio Defaults()
    {
        return new Audio
        {
            Music = 1.0f,
            SoundEffects = 1.0f
        };
    }
}