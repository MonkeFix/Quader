using System.Runtime.InteropServices;

namespace ColdClearNet;

[StructLayout(LayoutKind.Sequential)]
public struct PlanPlacement
{
    public Piece Piece;
    public TspinStatus TSpin;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] ExpectedX;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] ExpectedY;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public int[] ClearedLines;
}