using System;
using Nez.Persistence;
using Nez.Persistence.Binary;

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

public class BoardHardDropInfo : IPersistable
{
    /// <summary>
    /// Gets timestamp of the hardDropInfo in UTC
    /// </summary>
    [JsonInclude]
    public double Timestamp { get; set; }
    /// <summary>
    /// Gets total lines cleared by the hardDropInfo
    /// </summary>
    [JsonInclude]
    public int LinesCleared { get; set; }
    /// <summary>
    /// Gets flagged board hardDropInfo type. Use <b>BoardMoveType.HasFlag()</b> or bitwise OR operator to check the types applied
    /// </summary>
    [JsonInclude] public BoardHardDropInfoModificators Modificators { get; set; }
    /// <summary>
    /// Gets current back-to-back status. Back-to-back increments only if the last hardDropInfo was any T-Spin or Quad, in every other case it resets back to zero
    /// </summary>
    [JsonInclude] public int BackToBack { get; set; }
    /// <summary>
    /// Gets current combo. Note, that combo starts counting with every line clear
    /// </summary>
    [JsonInclude] public int Combo { get; set; }
    /// <summary>
    /// Gets whether or not was the hardDropInfo successful. If not, it usually means that the player just lost
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

    public void Recover(IPersistableReader reader)
    {
        Timestamp = reader.ReadDouble();
        LinesCleared = reader.ReadInt();
        Modificators = (BoardHardDropInfoModificators)reader.ReadInt();
        BackToBack = reader.ReadInt();
        Combo = reader.ReadInt();
        Success = reader.ReadBool();
        Attack = reader.ReadInt();
    }

    public void Persist(IPersistableWriter writer)
    {
        writer.Write(Timestamp);
        writer.Write(LinesCleared);
        writer.Write((int)Modificators);
        writer.Write(BackToBack);
        writer.Write(Combo);
        writer.Write(Success);
        writer.Write(Attack);
    }
}