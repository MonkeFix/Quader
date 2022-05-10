using System;

namespace Quader.Engine;

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
    public DateTime Timestamp { get; set; }
    public int LinesCleared { get; set; }
    public BoardMoveType Type { get; set; }
    public int BackToBack { get; set; }
    public int Combo { get; set; }
}