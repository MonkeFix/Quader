using Quader.Engine.Scoring;

namespace Quader.Engine.Replays;

public struct HardDropInfo
{
    public int LinesCleared;
    public TSpinStatus TSpinStatus;
    public LastMoveType LastMoveType;
    public int OccupiedCellsLeft;
}