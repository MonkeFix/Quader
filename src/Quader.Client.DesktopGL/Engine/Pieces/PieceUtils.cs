using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Quader.Engine.RotationEncoder;

namespace Quader.Engine.Pieces
{
    public static class PieceUtils
    {
        public static readonly Color ColorI = new (5, 195, 195);
        public static readonly Color ColorZ = new (211, 12, 37);
        public static readonly Color ColorS = new (5, 211, 5);
        public static readonly Color ColorL = new (237, 133, 1);
        public static readonly Color ColorJ = new (4, 24, 208);
        public static readonly Color ColorT = new (141, 16, 191);
        public static readonly Color ColorO = new (217, 205, 4);

        public static readonly Color ColorGarbage = new (102, 102, 102);

        public static readonly Dictionary<PieceRotationType, Point[]> DefaultWallKickData = new()
        {
            { PieceRotationType.SpawnToRight, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, 1), new Point(0, -2), new Point(-1, -2) } },
            { PieceRotationType.RightToSpawn, new[] { new Point(0, 0), new Point(1, 0), new Point(1, -1), new Point(0, 2), new Point(1, 2) } },
            { PieceRotationType.RightToDeg180, new[] { new Point(0, 0), new Point(1, 0), new Point(1, -1), new Point(0, 2), new Point(1, 2) } },
            { PieceRotationType.Deg180ToRight, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, 1), new Point(0, -2), new Point(-1, -2) } },
            { PieceRotationType.Deg180ToLeft, new[] { new Point(0, 0), new Point(1, 0), new Point(1, 1), new Point(0, -2), new Point(1, -2) } },
            { PieceRotationType.LeftToDeg180, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, -1), new Point(0, 2), new Point(-1, 2) } },
            { PieceRotationType.LeftToSpawn, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, -1), new Point(0, 2), new Point(-1, 2) } }, 
            { PieceRotationType.SpawnToLeft, new[] { new Point(0, 0), new Point(1, 0), new Point(1, 1), new Point(0, -2), new Point(1, -2) } }
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
            {PieceRotationType.SpawnToLeft, new [] {new Point(0, 0), new Point(-1, 0), new Point( 2, 0), new Point(-1, 2), new Point( 2,-1)}}
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
            { PieceRotationType.SpawnToLeft, new[] { new Point(0, 0), } }
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
                default:
                    return ColorGarbage;
            }
        }
        public static Color GetColorByBoardPieceType(BoardPieceType type)
        {
            return GetColorByPieceType((PieceType)(int)type);
        }
    }
}