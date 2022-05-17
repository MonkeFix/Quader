using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez.Persistence;

namespace Quader.Engine.Pieces
{
    public class PieceSettings
    {
        [JsonInclude]
        public Color ColorI { get; set; } = new(49, 178, 131);
        [JsonInclude]
        public Color ColorZ { get; set; } = new(179, 51, 58);
        [JsonInclude]
        public Color ColorS { get; set; } = new(129, 177, 48);
        [JsonInclude]
        public Color ColorL { get; set; } = new(178, 98, 49);
        [JsonInclude]
        public Color ColorJ { get; set; } = new(82, 57, 206);
        [JsonInclude]
        public Color ColorT { get; set; } = new(165, 62, 155);
        [JsonInclude]
        public Color ColorO { get; set; } = new(178, 153, 49);

        [JsonInclude]
        public Color ColorGarbage { get; set; } = new(102, 102, 102);

        [JsonInclude]
        public Dictionary<PieceRotationType, Point[]> DefaultWallKickData { get; set; } = new()
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

        [JsonInclude]
        public Dictionary<PieceRotationType, Point[]> PieceIWallKickData { get; set; } = new()
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

        [JsonInclude]
        public Dictionary<PieceRotationType, Point[]> PieceOWallKickData { get; set; } = new()
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

        public Dictionary<PieceType, Point[]> DefaultPieceCellPositioning { get; set; } = new()
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
            { PieceType.Pixel, new[] { Point.Zero } },
        };
    }

    public static class PieceUtils
    {
        public static PieceSettings? PieceSettings { get; set; }

        public static Color GetColorByPieceType(PieceType type)
        {
            if (PieceSettings == null)
                throw new ArgumentNullException(nameof(PieceSettings), "PieceSettings object is not initialized");

            switch (type)
            {
                case PieceType.I: return PieceSettings.ColorI;
                case PieceType.O: return PieceSettings.ColorO;
                case PieceType.T: return PieceSettings.ColorT;
                case PieceType.L: return PieceSettings.ColorL;
                case PieceType.J: return PieceSettings.ColorJ;
                case PieceType.S: return PieceSettings.ColorS;
                case PieceType.Z: return PieceSettings.ColorZ;
                case PieceType.Pixel: return PieceSettings.ColorGarbage;
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }

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

        /*public static Color GetColorByBoardCell(BoardCellType type)
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
        }*/
    }
}