using System;
using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public class Piece
    {
        public PieceType Type { get; }
        public int X { get; set; }
        public int Y { get; set; }

        public Color Color { get; }

        public float AbsoluteY { get; set; }

        internal bool[,] PieceTable { get; }

        public Piece(PieceType type)
        {
            Type = type;
            PieceTable = GetPieceTable();
            Color = PieceUtils.GetColorByPieceType(Type);
        }

        public bool[,] GetPieceTable() => PieceUtils.GetPieceTable(Type);
    }
}