using System;
using System.Runtime.CompilerServices;
using Microsoft.Xna.Framework;

namespace Quader.Engine;

public static class BoardUtils
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Point[] AdjustPositions(Point[] data, Point offset)
    {
        // TODO: Get rid of this method and perform all calculations in the PieceBase class on demand
        //  as this method takes a lot of memory
        var newData = new Point[data.Length];

        for (int i = 0; i < data.Length; i++)
        {
            newData[i] = new Point(data[i].X + offset.X, data[i].Y + offset.Y);
        }

        return newData;
    }

    public static void AdjustPositions(Span<Point> data, Point offset)
    {
        for (int i = 0; i < data.Length; i++)
        {
            var p = data[i];
            data[i] = new Point(p.X + offset.X, p.Y + offset.Y);
        }
    }
}