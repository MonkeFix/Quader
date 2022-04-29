using System;
using System.Text;
using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public static class PieceUtils
    {
        public static readonly Color ColorI = new Color(5, 195, 195);
        public static readonly Color ColorZ = new Color(211, 12, 37);
        public static readonly Color ColorS = new Color(5, 211, 5);
        public static readonly Color ColorL = new Color(237, 133, 1);
        public static readonly Color ColorJ = new Color(4, 24, 208);
        public static readonly Color ColorT = new Color(141, 16, 191);
        public static readonly Color ColorO = new Color(217, 205, 4);

        public static readonly Color ColorGarbage = new Color(102, 102, 102);

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
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }

        public static bool[,] GetPieceTable(PieceType type)
        {
            switch (type)
            {
                case PieceType.I:
                    return new[,]
                    {
                        { true, true, true, true }
                    };
                case PieceType.O:
                    return new[,]
                    {
                        { true,  true },
                        { true,  true }

                    };
                case PieceType.T:
                    return new[,]
                    {
                        { false, true, false,  },
                        { true, true, true,  },
                    };
                case PieceType.L:
                    return new[,]
                    {
                        { false, false, true,  },
                        { true, true, true,  },
                    };
                case PieceType.J:
                    return new[,]
                    {
                        { true,  false, false,  },
                        { true,  true,  true,  },
                    };
                case PieceType.S:
                    return new[,]
                    {
                        { false, true, true,  },
                        { true, true, false,  },
                    };
                case PieceType.Z:
                    return new[,]
                    {
                        { true, true, false,  },
                        { false, true, true, },
                    };
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }

        public static string[] GetPieceTableStr(PieceType type)
        {
            return type switch
            {
                PieceType.I => new[] { "....", "IIII", "....", "...." },
                PieceType.O => new[] { ".OO.", ".OO.", "....", "...." },
                PieceType.T => new[] { ".T..", "TTT.", "....", "...." },
                PieceType.L => new[] { "..L.", "LLL.", "....", "...." },
                PieceType.J => new[] { "J...", "JJJ.", "....", "...." },
                PieceType.S => new[] { ".SS.", "SS..", "....", "...." },
                PieceType.Z => new[] { "ZZ..", ".ZZ.", "....", "...." },
                _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
            };
        }

        public static Color GetColorByBoardPieceType(BoardPieceType type)
        {
            return GetColorByPieceType((PieceType)(int)type);
        }
    }
}