using System;
using Nez.Persistence;

namespace Quader.Engine.Replays;

[Flags]
// ReSharper disable InconsistentNaming
public enum BoardMoveModificators
{
    None = 0,

    [MoveType("T-Spin Mini")]
    TSpinMini = 1 << 1,
    [MoveType("T-Spin")]
    TSpin = 1 << 2,

    [MoveType("Single")]
    Single = 1 << 3,
    [MoveType("Double")]
    Double = 1 << 4,
    [MoveType("Triple")]
    Triple = 1 << 5,
    [MoveType("Quad")]
    Quad = 1 << 6,

    [MoveType("All Clear")]
    AllClear = 1 << 7,

    [MoveType("B2B")]
    BackToBack1 = 1 << 8,
    [MoveType("B2B")]
    BackToBack2 = 1 << 9,
    [MoveType("B2B")]
    BackToBack3 = 1 << 10,
    [MoveType("B2B")]
    BackToBack4 = 1 << 11,
    [MoveType("B2B")]
    BackToBack5 = 1 << 12,

    [MoveType("Combo")]
    Combo1 = 1 << 13,
    [MoveType("Combo")]
    Combo2 = 1 << 14,
    [MoveType("Combo")]
    Combo3 = 1 << 15,
    [MoveType("Combo")]
    Combo4 = 1 << 16,
    [MoveType("Combo")]
    Combo5 = 1 << 17
}
// ReSharper enable InconsistentNaming

public struct BoardMove
{
    /// <summary>
    /// Gets timestamp of the move in UTC
    /// </summary>
    [JsonInclude]
    public float Timestamp { get; set; }
    /// <summary>
    /// Gets total lines cleared by the move
    /// </summary>
    [JsonInclude]
    public int LinesCleared { get; set; }
    /// <summary>
    /// Gets flagged board move type. Use <b>BoardMoveType.HasFlag()</b> or bitwise OR operator to check the types applied
    /// </summary>
    [JsonInclude] public BoardMoveModificators Modificators { get; set; }
    /// <summary>
    /// Gets current back-to-back status. Back-to-back increments only if the last move was any T-Spin or Quad, in every other case it resets back to zero
    /// </summary>
    [JsonInclude] public int BackToBack { get; set; }
    /// <summary>
    /// Gets current combo. Note, that combo starts counting with every line clear
    /// </summary>
    [JsonInclude] public int Combo { get; set; }
    /// <summary>
    /// Gets whether or not was the move successful. If not, it usually means that the player just lost
    /// </summary>
    [JsonInclude] public bool Success { get; set; }
    /// <summary>
    /// Gets outgoing attack
    /// </summary>
    [JsonInclude] public int Attack { get; set; }

    public override string ToString()
    {
        return $"{Timestamp}: LC: {LinesCleared}, Mods: {Modificators}, B2B: {BackToBack}, Combo: {Combo}, Success: {Success}, Attack: {Attack}";
    }
}