using System;
using System.Collections.Generic;
using System.Runtime.Versioning;
using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;
using Quader.Engine.RotationEncoder;

namespace Quader.Engine
{
    public enum Rotation
    {
        Clockwise,
        CounterClockwise,
        Deg180
    }
    
    public class Piece
    {
        public PieceType Type { get; }
        public int X { get; set; }
        public int Y { get; set; }
        public int Width { get; private set; }
        public int Height { get; private set; }

        public Color Color { get; }

        public float AbsoluteY { get; set; }

        // internal bool[,] PieceTable { get; private set; }
        
        public string[] PieceTable { get; private set; }
        public PieceStartPosition Position { get; private set; } = PieceStartPosition.Initial;

        //public Dictionary<PieceStartPosition, string[][]> TestsMap = new();
        
        public RotationPositionEncoding RotationPositionEncoding { get; }

        public RotationEncoding RotationEncoding => RotationPositionEncoding.PositionEncodings[Position];
        
        //public string[][] Tests => TestsMap[Position];

        internal Piece(PieceType type, RotationPositionEncoding rotationPositionEncoding)
        {
            Type = type;
            RotationPositionEncoding = rotationPositionEncoding;
            PieceTable = RotationPositionEncoding.PositionEncodings[Position].InitialEncoding;
            // PieceTable = GetPieceTable();
            Color = PieceUtils.GetColorByPieceType(Type);


            Height = PieceTable.Length;
            Width = PieceTable[0].Length;
            /*switch (type)
            {
                case PieceType.I:
                    Width = Height = 4;
                    break;
                case PieceType.O:
                    Width = Height = 2;
                    break;
                case PieceType.T:
                case PieceType.L:
                case PieceType.J:
                case PieceType.S:
                case PieceType.Z:
                    Width = Height = 3;
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }*/

            /*
            Width = PieceTable.GetLength(1);
            Height = PieceTable.GetLength(0);
            */
        }

        public bool DrawAt(int x, int y)
        {
            return PieceTable[y][x] == ConverterOptions.FilledChar;
        }

        internal void Rotate(Rotation rotation, Func<string[][], string[]?> tableFunc)
        {
            string[]? table;
            
            if (rotation == Rotation.Clockwise)
            {
                table = tableFunc(RotationEncoding.TestsClockwise);
            }
            else if (rotation == Rotation.CounterClockwise)
            {
                table = tableFunc(RotationEncoding.TestsCounterClockwise);
            }
            else
            {
                // TODO: Fix this
                table = tableFunc(RotationEncoding.TestsClockwise);
            }

            if (table == null)
                return;

            switch (rotation)
            {
                case Rotation.Clockwise:
                    RotateClockwise();
                    break;
                case Rotation.CounterClockwise:
                    RotateCounterClockwise();
                    break;
                case Rotation.Deg180:
                    Rotate180();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
            }

            
            PieceTable = table;
        }

        private void RotateClockwise()
        {
            Position = Position switch
            {
                PieceStartPosition.Initial => PieceStartPosition.RotationClockwise,
                PieceStartPosition.RotationClockwise => PieceStartPosition.Rotation180Deg,
                PieceStartPosition.Rotation180Deg => PieceStartPosition.RotationCounterClockwise,
                PieceStartPosition.RotationCounterClockwise => PieceStartPosition.Initial,
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        private void RotateCounterClockwise()
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

        private void Rotate180()
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

        /*private static bool[,] RotateArrayClockwise(bool[,] src)
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
        }*/
    }
}