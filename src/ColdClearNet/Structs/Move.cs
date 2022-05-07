using System.Runtime.InteropServices;

namespace ColdClearNet;

[StructLayout(LayoutKind.Sequential)]
public class Move
{
    [MarshalAs(UnmanagedType.U1)] public bool Hold;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] ExpectedX = null!;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] ExpectedY = null!;

    public byte MovementCount;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32)]
    public Movement[] Movements = null!;

    public uint Nodes;
    public uint Depth;
    public uint OriginalRank;
}