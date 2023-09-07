using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Quader.Engine.Settings;

[StructLayout(LayoutKind.Sequential)]
public class AttackSettings
{
    public int Lines0;
    public int Lines1;
    public int Lines2;
    public int Lines3;
    public int Lines4;
    public int TSpinSingle;
    public int TSpinDouble;
    public int TSpinTriple;
    public int TSpinSingleMini;
    public int AllClear;
    [MarshalAs(UnmanagedType.ByValArray)]
    public int[] BackToBacks;
    [MarshalAs(UnmanagedType.ByValArray)]
    public int[] Combos;
    public int GarbageDelayMs;
}