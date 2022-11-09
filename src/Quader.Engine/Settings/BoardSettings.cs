using Nez.Persistence;

namespace Quader.Engine.Settings;

public class BoardSettings
{
    [JsonInclude] public int BoardWidth;
    [JsonInclude] public int BoardHeight;
}