using System;
using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public class Piece
    {
        public PieceType Type { get; }
        public int X { get; set; }
        public int Y { get; set; }

        internal bool[,] PieceTable { get; }

        public Piece(PieceType type)
        {
            Type = type;
            PieceTable = GetPieceTable();
        }

        public bool[,] GetPieceTable() => Piece.GetPieceTable(Type);

        public static bool[,] GetPieceTable(PieceType type)
        {
            switch (type)
            {
                case PieceType.I:
                    return new [,]
                    {
                        { false, false, false, false },
                        { true, true, true, true },
                        { false, false, false, false },
                        { false, false, false, false }
                    };
                case PieceType.O:
                    return new[,]
                    {
                        { true,  true },
                        { true,  true  },
                    };
                case PieceType.T:
                    return new[,]
                    {
                        { false, true, false },
                        { true, true, true },
                        { false, false, false },
                    };
                case PieceType.L:
                    return new[,]
                    {
                        { false, false, true },
                        { true, true, true },
                        { false, false, false },
                    };
                case PieceType.J:
                    return new[,]
                    {
                        { true,  false, false },
                        { true,  true,  true },
                        { false, false, false },
                        { false, false, false }
                    };
                case PieceType.S:
                    return new[,]
                    {
                        { false, true, true },
                        { true, true, false },
                        { false, false, false },
                    };
                case PieceType.Z:
                    return new[,]
                    {
                        { true, true, false },
                        { false, true, true},
                        { false, false, false },
                    };
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }
    }
}