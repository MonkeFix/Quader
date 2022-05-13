using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;

namespace Quader.Engine.Pieces
{
    public static class PieceUtils
    {
        public static readonly Color ColorI = new (49, 178, 131);
        public static readonly Color ColorZ = new (179, 51, 58);
        public static readonly Color ColorS = new (129, 177, 48);
        public static readonly Color ColorL = new (178, 98, 49);
        public static readonly Color ColorJ = new (82, 57, 206);
        public static readonly Color ColorT = new (165, 62, 155);
        public static readonly Color ColorO = new (178, 153, 49);

        public static readonly Color ColorGarbage = new (102, 102, 102);

        public static readonly Dictionary<PieceRotationType, Point[]> DefaultWallKickData = new()
        {
            { PieceRotationType.SpawnToRight, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, 1), new Point(0, -2), new Point(-1, -2) } },
            { PieceRotationType.RightToSpawn, new[] { new Point(0, 0), new Point(1, 0), new Point(1, -1), new Point(0, 2), new Point(1, 2) } },
            { PieceRotationType.RightToDeg180, new[] { new Point(0, 0), new Point(1, 0), new Point(1, -1), new Point(0, 1), new Point(0, 2)  } },
            { PieceRotationType.Deg180ToRight, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, 1), new Point(0, -2), new Point(-1, -2) } },
            { PieceRotationType.Deg180ToLeft, new[] { new Point(0, 0), new Point(1, 0), new Point(1, 1), new Point(0, -2), new Point(1, -2) } },
            { PieceRotationType.LeftToDeg180, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, -1), new Point(0, 2), new Point(-1, 2) } },
            { PieceRotationType.LeftToSpawn, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, -1), new Point(0, 2), new Point(-1, 2) } }, 
            { PieceRotationType.SpawnToLeft, new[] { new Point(0, 0), new Point(1, 0), new Point(1, 1), new Point(0, -2), new Point(1, -2) } },
            { PieceRotationType.SpawnToDeg180, new[] { new Point(0, 0), new Point(0, 1), new Point(0, 2) } },
            { PieceRotationType.Deg180ToSpawn, new[] { new Point(0, 0), new Point(0, -1), new Point(0, -2) } },
        };
        
        public static readonly Dictionary<PieceRotationType, Point[]> PieceIWallKickData = new()
        {
            {PieceRotationType.SpawnToRight, new [] {new Point(0, 0), new Point(-2, 0), new Point( 1, 0), new Point(-2,-1), new Point( 1, 2)}},
            {PieceRotationType.RightToSpawn, new [] {new Point(0, 0), new Point( 2, 0), new Point(-1, 0), new Point( 2, 1), new Point(-1,-2)}},
            {PieceRotationType.RightToDeg180, new [] {new Point(0, 0), new Point(-1, 0), new Point( 2, 0), new Point(-1, 2), new Point( 2,-1)}},
            {PieceRotationType.Deg180ToRight, new [] {new Point(0, 0), new Point( 1, 0), new Point(-2, 0), new Point( 1,-2), new Point(-2, 1)}},
            {PieceRotationType.Deg180ToLeft, new [] {new Point(0, 0), new Point( 2, 0), new Point(-1, 0), new Point( 2, 1), new Point(-1,-2)}},
            {PieceRotationType.LeftToDeg180, new [] {new Point(0, 0), new Point(-2, 0), new Point( 1, 0), new Point(-2,-1), new Point( 1, 2)}},
            {PieceRotationType.LeftToSpawn, new [] {new Point(0, 0), new Point( 1, 0), new Point(-2, 0), new Point( 1,-2), new Point(-2, 1)}},
            {PieceRotationType.SpawnToLeft, new [] {new Point(0, 0), new Point(-1, 0), new Point( 2, 0), new Point(-1, 2), new Point( 2,-1)}},
            { PieceRotationType.SpawnToDeg180, new[] { new Point(0, 0), new Point(0, 1), new Point(0, 2) } },
            { PieceRotationType.Deg180ToSpawn, new[] { new Point(0, 0), new Point(0, -1), new Point(0, -2) } },
        };

        public static readonly Dictionary<PieceRotationType, Point[]> PieceOWallKickData = new()
        {
            { PieceRotationType.SpawnToRight, new[] { new Point(0, 0) } },
            { PieceRotationType.RightToSpawn, new[] { new Point(0, 0) } },
            { PieceRotationType.RightToDeg180, new[] { new Point(0, 0) } },
            { PieceRotationType.Deg180ToRight, new[] { new Point(0, 0) } },
            { PieceRotationType.Deg180ToLeft, new[] { new Point(0, 0), } },
            { PieceRotationType.LeftToDeg180, new[] { new Point(0, 0), } },
            { PieceRotationType.LeftToSpawn, new[] { new Point(0, 0), } },
            { PieceRotationType.SpawnToLeft, new[] { new Point(0, 0), } },
            { PieceRotationType.SpawnToDeg180, new[] { new Point(0, 0), } },
            { PieceRotationType.Deg180ToSpawn, new[] { new Point(0, 0), } },
        };

        public static Color GetColorByPieceType(PieceType type)
        {
            switch (type)
            {
                case PieceType.I: return ColorI;
                case PieceType.O: return ColorO;
                case PieceType.T: return ColorT;
                case PieceType.L: return ColorL;
                case PieceType.J: return ColorJ;
                case PieceType.S: return ColorS;
                case PieceType.Z: return ColorZ;
                case PieceType.Pixel: return ColorGarbage;
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }
        public static Color GetColorByBoardCell(BoardCellType type)
        {
            switch (type)
            {
                case BoardCellType.None:
                    return Color.Transparent;
                case BoardCellType.I:
                    return ColorI;
                case BoardCellType.O:
                    return ColorO;
                case BoardCellType.T:
                    return ColorT;
                case BoardCellType.L:
                    return ColorL;
                case BoardCellType.J:
                    return ColorJ;
                case BoardCellType.S:
                    return ColorS;
                case BoardCellType.Z:
                    return ColorZ;
                case BoardCellType.Garbage:
                    return ColorGarbage;
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }
    }
}