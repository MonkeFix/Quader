using System;
namespace Quader.Engine.Replays;

[Flags]
// ReSharper disable InconsistentNaming
public enum BoardHardDropInfoModificators
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

public class BoardHardDropInfo
{
    /// <summary>
    /// Gets timestamp of the hardDropInfo in UTC
    /// </summary>
    public double Timestamp;

    /// <summary>
    /// Gets total lines cleared by the hardDropInfo
    /// </summary>
    public int LinesCleared;

    /// <summary>
    /// Gets flagged board hardDropInfo type. Use <b>BoardMoveType.HasFlag()</b> or bitwise OR operator to check the types applied
    /// </summary>
    public BoardHardDropInfoModificators Modificators;

    /// <summary>
    /// Gets current back-to-back status. Back-to-back increments only if the last hardDropInfo was any T-Spin or Quad, in every other case it resets back to zero
    /// </summary>
    public int BackToBack;

    /// <summary>
    /// Gets current combo. Note, that combo starts counting with every line clear
    /// </summary>
    public int Combo;

    /// <summary>
    /// Gets whether or not was the hardDropInfo successful. If not, it usually means that the player just lost
    /// </summary>
    public bool Success;

    /// <summary>
    /// Gets outgoing attack
    /// </summary>
    public int Attack;

    public override string ToString()
    {
        return $"{Timestamp}: LC: {LinesCleared}, Mods: {Modificators}, B2B: {BackToBack}, Combo: {Combo}, Success: {Success}, Attack: {Attack}";
    }
}