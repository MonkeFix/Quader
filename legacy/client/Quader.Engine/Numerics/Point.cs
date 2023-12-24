using System.Runtime.InteropServices;

namespace Quader.Engine.Numerics;

[StructLayout(LayoutKind.Sequential)]
public struct Point
{
    public int X;
    public int Y;

    public Point()
    {
        
    }

    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }
}