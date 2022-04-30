using System;
using System.Collections.Generic;
using System.Runtime.Versioning;
using Microsoft.Xna.Framework;
using Quader.Engine.RotationEncoder;

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
        
        public string[] Table { get; private set; }

        public PieceStartPosition Position { get; private set; } = PieceStartPosition.Initial;

        public Dictionary<PieceStartPosition, string[][]> TestsMap = new();

        public string[][] Tests => TestsMap[Position];

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
            /*PieceTable = RotateArrayClockwise(PieceTable);
            Width = PieceTable.GetLength(1);
            Height = PieceTable.GetLength(0);*/

            Position = Position switch
            {
                PieceStartPosition.Initial => PieceStartPosition.RotationClockwise,
                PieceStartPosition.RotationClockwise => PieceStartPosition.Rotation180Deg,
                PieceStartPosition.RotationCounterClockwise => PieceStartPosition.Initial,
                PieceStartPosition.Rotation180Deg => PieceStartPosition.RotationCounterClockwise,
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        public void RotateCounterClockwise()
        {
            Position = Position switch
            {
                PieceStartPosition.Initial => PieceStartPosition.RotationCounterClockwise,
                PieceStartPosition.RotationClockwise => PieceStartPosition.Initial,
                PieceStartPosition.RotationCounterClockwise => PieceStartPosition.Rotation180Deg,
                PieceStartPosition.Rotation180Deg => PieceStartPosition.RotationClockwise,
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        public void Rotate180()
        {
            Position = Position switch
            {
                PieceStartPosition.Initial => PieceStartPosition.Rotation180Deg,
                PieceStartPosition.RotationClockwise => PieceStartPosition.RotationCounterClockwise,
                PieceStartPosition.RotationCounterClockwise => PieceStartPosition.RotationClockwise,
                PieceStartPosition.Rotation180Deg => PieceStartPosition.Initial,
                _ => throw new ArgumentOutOfRangeException()
            };
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