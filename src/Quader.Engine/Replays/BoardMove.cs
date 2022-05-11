using System;

namespace Quader.Engine.Replays;

[Flags]
// ReSharper disable InconsistentNaming
public enum BoardMoveType
{
    None = 0,
    TSpinSingleMini = 1,
    TSpinDoubleMini = 2,
    TSpinSingle = 4,
    TSpinDouble = 8,
    TSpinTriple = 16,
    TSpin = 32,
    Single = 64,
    Double = 128,
    Triple = 256,
    Quad = 512,
    AllClear = 1024,
    BackToBack = 2028,
    Combo = 4096,
}
// ReSharper enable InconsistentNaming

public struct BoardMove
{
    /// <summary>
    /// Gets timestamp of the move in UTC
    /// </summary>
    public DateTime Timestamp { get; set; }
    /// <summary>
    /// Gets total lines cleared by the move
    /// </summary>
    public int LinesCleared { get; set; }
    /// <summary>
    /// Gets flagged board move type. Use <b>BoardMoveType.HasFlag()</b> or bitwise OR operator to check the types applied
    /// </summary>
    public BoardMoveType Type { get; set; }
    /// <summary>
    /// Gets current back-to-back status. Back-to-back increments only if the last move was any T-Spin or Quad, in every other case it resets back to zero
    /// </summary>
    public int BackToBack { get; set; }
    /// <summary>
    /// Gets current combo. Note, that combo starts counting with every line clear
    /// </summary>
    public int Combo { get; set; }
    /// <summary>
    /// Gets whether or not was the move successful. If not, it usually means that the player just lost
    /// </summary>
    public bool Success { get; set; }
}