using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public class PieceMovedEventArgs
    {
        public Point Offset { get; set; }
        public Point NewPosition { get; set; }

        public PieceMovedEventArgs(Point offset, Point newPosition)
        {
            Offset = offset;
            NewPosition = newPosition;
        }
    }
}