using Microsoft.Xna.Framework;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Skinning;

public static class PieceColors
{
    public static Color ColorI = new(49, 178, 131);
    public static Color ColorZ = new(179, 51, 58);
    public static Color ColorS = new(129, 177, 48);
    public static Color ColorL = new(178, 98, 49);
    public static Color ColorJ = new(82, 57, 206);
    public static Color ColorT = new(165, 62, 155);
    public static Color ColorO = new(178, 153, 49);
    public static Color ColorGarbage = new(102, 102, 102);
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

}