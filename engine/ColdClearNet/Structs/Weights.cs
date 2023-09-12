using System.Runtime.InteropServices;

namespace ColdClearNet;

[StructLayout(LayoutKind.Sequential)]
public struct Weights
{
    public int BackToBack;
    public int Bumpiness;
    public int BumpinessSq;
    public int RowTransitions;
    public int Height;
    public int TopHalf;
    public int TopQuarter;
    public int Jeopardy;
    public int CavityCells;
    public int CavityCellsSq;
    public int OverhangCells;
    public int OverhangCellsSq;
    public int CoveredCells;
    public int CoveredCellsSq;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public int[] TSlot;
    public int WellDepth;
    public int MaxWellDepth;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 10)]
    public int[] WellColumn;

    public int B2BClear;
    public int Clear1;
    public int Clear2;
    public int Clear3;
    public int Clear4;
    public int TSpin1;
    public int TSpin2;
    public int TSpin3;
    public int MiniTSpin1;
    public int MiniTSpin2;
    public int PerfectClear;
    public int ComboGarbage;
    public int MoveTime;
    public int WastedT;

    [MarshalAs(UnmanagedType.U1)]
    public bool UseBag;
    [MarshalAs(UnmanagedType.U1)]
    public bool TimedJeopardy;
    [MarshalAs(UnmanagedType.U1)]
    public bool StackPcDamage;
}