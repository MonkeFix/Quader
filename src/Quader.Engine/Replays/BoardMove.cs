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
    Single = 32,
    Double = 64,
    Triple = 128,
    Quad = 256,
    AllClear = 512,
    BackToBack = 1024,
    Combo = 2048,
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