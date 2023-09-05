using System;
using System.Collections.Generic;
using System.Drawing;
using System.Text.Json.Serialization;

namespace Quader.Engine.Pieces
{
    public class PieceSettings
    {
        public static Color ColorI = Color.FromArgb(255, 49, 178, 131);
        public static Color ColorZ = Color.FromArgb(255, 179, 51, 58);
        public static Color ColorS = Color.FromArgb(255, 129, 177, 48);
        public static Color ColorL = Color.FromArgb(255, 178, 98, 49);
        public static Color ColorJ = Color.FromArgb(255, 82, 57, 206);
        public static Color ColorT = Color.FromArgb(255, 165, 62, 155);
        public static Color ColorO = Color.FromArgb(255, 178, 153, 49);
        
        public static Color ColorGarbage = Color.FromArgb(255, 102, 102, 102);
        
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