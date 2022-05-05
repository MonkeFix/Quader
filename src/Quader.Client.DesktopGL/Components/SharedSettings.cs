using Microsoft.Xna.Framework;

namespace Quader.Components
{
    public static class SharedSettings
    {
        public static bool DrawPieceBoundingBox;
        public static bool DrawPieceOrigin = true;
        public static bool DrawPieceRotationTests = true;

        public static Point[]? CurrentTest;
    }
}