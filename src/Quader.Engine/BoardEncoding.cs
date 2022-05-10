using Nez.Persistence;

namespace Quader.Engine;

public class BoardEncoding
{
    [JsonInclude] 
    public string Code { get; set; } = null!;
    [JsonInclude]
    public int Width { get; set; }
    [JsonInclude]
    public int Height { get; set; }
    [JsonInclude]
    public int TotalHeight { get; set; }
}