using Nez.Persistence;

namespace Quader.Engine.Settings;

public class BoardSettings
{
    [JsonInclude]
    public int BoardWidth { get; set; }
    [JsonInclude]
    public int BoardHeight { get; set; }
}