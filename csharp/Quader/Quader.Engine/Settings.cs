namespace Quader.Engine;

public struct GravitySettings
{
    public float GravityConst;
    public float GravityBase;
    public float GravityIncrease;
    public float LockDelay;
    public float LockProlongAmount;

    public static GravitySettings Default => new GravitySettings
    {
        GravityBase = 0.8f,
        GravityConst = 0.0f,
        GravityIncrease = 0.007f,
        LockDelay = 1.0f,
        LockProlongAmount = 0.02f
    };
}

public struct AttackSettings
{
    public uint Lines0;
    public uint Lines1;
    public uint Lines2;
    public uint Lines3;
    public uint Lines4;

    // ReSharper disable InconsistentNaming
    public uint TSpinSingle;
    public uint TSpinDouble;
    public uint TSpinTriple;
    public uint TSpinSingleMini;
    // ReSharper restore InconsistentNaming

    public uint AllClear;
    public uint[] B2Bs;
    public uint[] Combos;
    public uint GarbageDelayMs;

    public static AttackSettings Default => new AttackSettings
    {
        Lines0 = 0,
        Lines1 = 0,
        Lines2 = 1,
        Lines3 = 2,
        Lines4 = 4,
        TSpinSingle = 2,
        TSpinDouble = 4,
        TSpinTriple = 6,
        TSpinSingleMini = 1,
        AllClear = 10,
        B2Bs = [1, 2, 3, 4, 5],
        Combos = [1, 2, 3, 4, 5],
        GarbageDelayMs = 1000
    };
}

public struct BoardSettings
{
    public int Width = 10;
    public int Height = 20;

    public BoardSettings()
    {
    }

    public int FullHeight => Height * 2;

    public static BoardSettings Default => new BoardSettings { Width = 10, Height = 20 };
}

public struct GameSettings
{
    public GravitySettings Gravity;
    public AttackSettings Attack;
    public BoardSettings Board;

    public static GameSettings Default => new GameSettings
    {
        Gravity = GravitySettings.Default,
        Attack = AttackSettings.Default,
        Board = BoardSettings.Default
    };
}