using System;
using System.Collections.Generic;
using System.Drawing;
using System.Text.Json.Serialization;

namespace Quader.Engine.Pieces
{
    public class PieceSettings
    {
        public static Dictionary<PieceRotationType, Point[]> DefaultWallKickData = new()
        {
            { PieceRotationType.SpawnToRight, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, 1), new Point(0, -2), new Point(-1, -2) } },
            { PieceRotationType.RightToSpawn, new[] { new Point(0, 0), new Point(1, 0), new Point(1, -1), new Point(0, 2), new Point(1, 2) } },
            { PieceRotationType.RightToDeg180, new[] { new Point(0, 0), new Point(1, 0), new Point(1, -1), new Point(0, 1), new Point(0, 2) } },
            { PieceRotationType.Deg180ToRight, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, 1), new Point(0, -2), new Point(-1, -2) } },
            { PieceRotationType.Deg180ToLeft, new[] { new Point(0, 0), new Point(1, 0), new Point(1, 1), new Point(0, -2), new Point(1, -2) } },
            { PieceRotationType.LeftToDeg180, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, -1), new Point(0, 2), new Point(-1, 2) } },
            { PieceRotationType.LeftToSpawn, new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, -1), new Point(0, 2), new Point(-1, 2) } },
            { PieceRotationType.SpawnToLeft, new[] { new Point(0, 0), new Point(1, 0), new Point(1, 1), new Point(0, -2), new Point(1, -2) } },
            { PieceRotationType.SpawnToDeg180, new[] { new Point(0, 0), new Point(0, 1), new Point(0, 2) } },
            { PieceRotationType.Deg180ToSpawn, new[] { new Point(0, 0), new Point(0, -1), new Point(0, -2) } },
        };

        public static Dictionary<PieceRotationType, Point[]> PieceIWallKickData = new()
        {
            { PieceRotationType.SpawnToRight, new[] { new Point(0, 0), new Point(-2, 0), new Point(1, 0), new Point(-2, -1), new Point(1, 2) } },
            { PieceRotationType.RightToSpawn, new[] { new Point(0, 0), new Point(2, 0), new Point(-1, 0), new Point(2, 1), new Point(-1, -2) } },
            { PieceRotationType.RightToDeg180, new[] { new Point(0, 0), new Point(-1, 0), new Point(2, 0), new Point(-1, 2), new Point(2, -1) } },
            { PieceRotationType.Deg180ToRight, new[] { new Point(0, 0), new Point(1, 0), new Point(-2, 0), new Point(1, -2), new Point(-2, 1) } },
            { PieceRotationType.Deg180ToLeft, new[] { new Point(0, 0), new Point(2, 0), new Point(-1, 0), new Point(2, 1), new Point(-1, -2) } },
            { PieceRotationType.LeftToDeg180, new[] { new Point(0, 0), new Point(-2, 0), new Point(1, 0), new Point(-2, -1), new Point(1, 2) } },
            { PieceRotationType.LeftToSpawn, new[] { new Point(0, 0), new Point(1, 0), new Point(-2, 0), new Point(1, -2), new Point(-2, 1) } },
            { PieceRotationType.SpawnToLeft, new[] { new Point(0, 0), new Point(-1, 0), new Point(2, 0), new Point(-1, 2), new Point(2, -1) } },
            { PieceRotationType.SpawnToDeg180, new[] { new Point(0, 0), new Point(0, 1), new Point(0, 2) } },
            { PieceRotationType.Deg180ToSpawn, new[] { new Point(0, 0), new Point(0, -1), new Point(0, -2) } },
        };

        public static Dictionary<PieceRotationType, Point[]> PieceOWallKickData = new()
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

        public static Dictionary<PieceType, Point[]> DefaultPieceCellPositioning { get; set; } = new()
        {
            { PieceType.I, new[] { new Point(-1, -1), new Point(-2, -1), new Point(1, -1), new Point(0, -1) } },
            { PieceType.S, new[] { new Point(0, 0), new Point(-1, 0), new Point(0, -1), new Point(1, -1) } },
            { PieceType.Z, new[] { new Point(0, 0), new Point(-1, -1), new Point(0, -1), new Point(1, 0) } },
            { PieceType.J, new[] { new Point(0, 0), new Point(1, 0), new Point(-1, 0), new Point(-1, -1) } },
            { PieceType.L, new[] { new Point(0, 0), new Point(-1, 0), new Point(1, 0), new Point(1, -1) } },
            { PieceType.O, new[] { new Point(0, 0), new Point(-1, 0), new Point(0, -1), new Point(-1, -1) } },
            {
                PieceType.T,
                new[]
                {
                    new Point(0, 0),
                    new Point(-1, 0),
                    new Point(1, 0),
                    new Point(0, -1)
                }
            },
            { PieceType.Pixel, new[] { Point.Empty } },
        };
    }

    public static class PieceUtils
    {
        public static PieceSettings? PieceSettings { get; set; }

        public static BoardCellType GetBoardCellTypeByPieceType(PieceType type)
        {
            switch (type)
            {
                case PieceType.I: return BoardCellType.I;
                case PieceType.O: return BoardCellType.O;
                case PieceType.T: return BoardCellType.T;
                case PieceType.L: return BoardCellType.L;
                case PieceType.J: return BoardCellType.J;
                case PieceType.S: return BoardCellType.S;
                case PieceType.Z: return BoardCellType.Z;
                case PieceType.Pixel: return BoardCellType.Garbage;
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }

        public static Point[] GetPiecePointsByType(PieceType type)
        {
            if (PieceSettings == null)
                throw new ArgumentNullException(nameof(PieceSettings), "PieceSettings object is not initialized");

            switch (type)
            {
                case PieceType.I:
                case PieceType.O:
                case PieceType.T:
                case PieceType.L:
                case PieceType.J:
                case PieceType.S:
                case PieceType.Z: 
                case PieceType.Pixel: return PieceSettings.DefaultPieceCellPositioning[type];
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }
    }
}