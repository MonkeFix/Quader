using System.Runtime.InteropServices;

namespace ColdClearNet;

[StructLayout(LayoutKind.Sequential)]
public struct Move
{
    [MarshalAs(UnmanagedType.U1)] public bool Hold;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] ExpectedX;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
    public byte[] ExpectedY;

    public byte MovementCount;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32)]
    public Movement[] Movements;

    public uint Nodes;
    public uint Depth;
    public uint OriginalRank;
}