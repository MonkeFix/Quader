using System;
using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public class Piece
    {
        public PieceType Type { get; }
        public int X { get; set; }
        public int Y { get; set; }
        public int Width { get; private set; }
        public int Height { get; private set; }

        public Color Color { get; }

        public float AbsoluteY { get; set; }

        internal bool[,] PieceTable { get; private set; }

        public Piece(PieceType type)
        {
            Type = type;
            PieceTable = GetPieceTable();
            Color = PieceUtils.GetColorByPieceType(Type);

            Width = PieceTable.GetLength(1);
            Height = PieceTable.GetLength(0);
        }

        public bool[,] GetPieceTable() => PieceUtils.GetPieceTable(Type);

        public void RotateClockwise()
        {
            PieceTable = RotateArrayClockwise(PieceTable);
            Width = PieceTable.GetLength(1);
            Height = PieceTable.GetLength(0);
        }

        public void RotateCounterClockwise()
        {

        }

        public void Rotate180()
        {

        }

        private static bool[,] RotateArrayClockwise(bool[,] src)
        {
            int width;
            int height;
            bool[,] dst;

            width = src.GetUpperBound(0) + 1;
            height = src.GetUpperBound(1) + 1;
            dst = new bool[height, width];

            for (int row = 0; row < height; row++)
            {
                for (int col = 0; col < width; col++)
                {
                    int newRow;
                    int newCol;

                    newRow = col;
                    newCol = height - (row + 1);

                    dst[newCol, newRow] = src[col, row];
                }
            }

            return dst;
        }
    }
}