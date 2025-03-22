namespace Quader.Engine.Scoring;

[Flags]
public enum DamageMods : uint
{
    None = 0,

    TSpinMini = 1,
    TSpinFull = 1 << 1,

    Single = 1 << 2,
    Double = 1 << 3,
    Triple = 1 << 4,
    Quad = 1 << 5,

    AllClear = 1 << 6,

    B2B1 = 1 << 7,
    B2B2 = 1 << 8,
    B2B3 = 1 << 9,
    B2B4 = 1 << 10,
    B2B5 = 1 << 11,

    Combo1 = 1 << 12,
    Combo2 = 1 << 13,
    Combo3 = 1 << 14,
    Combo4 = 1 << 15,
    Combo5 = 1 << 16,
}