using System.Runtime.InteropServices;

namespace ColdClearNet;

[StructLayout(LayoutKind.Sequential)]
public struct Options
{
    public MovementMode Mode;
    public SpawnRule SpawnRule;
    public PcPriority PcLoop;
    public uint MinNodes;
    public uint MaxNodes;
    public uint Threads;
    [MarshalAs(UnmanagedType.U1)]
    public bool UseHold;
    [MarshalAs(UnmanagedType.U1)]
    public bool Speculate;
}