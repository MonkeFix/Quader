using Nez.Persistence;

namespace Quader.Engine.Replays;

public struct ReplayMove
{
    [JsonInclude]
    public BoardMove? BoardMove { get; set; }
    [JsonInclude]
    public ReplayMoveType Type { get; set; }
    [JsonInclude]
    public long Tick { get; set; }
    [JsonInclude]
    public ReplayMoveInfo Info { get; set; }
}

public struct ReplayMoveInfo
{
    [JsonInclude]
    public int SoftDropFactor { get; set; }
    [JsonInclude]
    public int MoveLeftFactor { get; set; }
    [JsonInclude]
    public int MoveRightFactor { get; set; }
}