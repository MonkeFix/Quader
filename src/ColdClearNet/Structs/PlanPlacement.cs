using System.Runtime.InteropServices;

namespace ColdClearNet;

public class PlanPlacement
{
    public Piece Piece;
    public TspinStatus TSpin;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public bool[] ExpectedX = null!;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public bool[] ExpectedY = null!;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public int[] ClearedLines = null!;
}