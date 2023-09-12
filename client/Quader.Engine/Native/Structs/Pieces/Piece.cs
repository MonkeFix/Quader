using System.Collections.Generic;
using System.Drawing;
using System.Runtime.InteropServices;
using Quader.Engine.Pieces;

namespace Quader.Engine.Native.Structs;

[StructLayout(LayoutKind.Sequential)]
internal struct Piece
{
    public PieceType Type;
    public BoardCellType BoardCellType;
    public OffsetType OffsetType;
    public Rectangle Bounds;
    public Color BaseColor;
    public Point[] CurrentPos;
    public int X;
    public int Y;
    public PieceStartPosition CurrentRotation;
    // public Dictionary<PieceRotationType, Point[]> WallKickData;
    public void Rotate(){}
    //public Point[] GetWallKickData(PieceRotationType rotationType) {}

}

