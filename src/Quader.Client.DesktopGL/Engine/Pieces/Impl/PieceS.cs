using Microsoft.Xna.Framework;

namespace Quader.Engine.Pieces.Impl
{
    public class PieceS : PieceBase
    {
        public override PieceType Type => PieceType.S;
        public override Point[] SpawnPos { get; }
        public override Point[] RightPos { get; }
        public override Point[] Deg180Pos { get; }
        public override Point[] LeftPos { get; }

        public PieceS()
        {
            SpawnPos = new[] { new Point(0, 0), new Point(-1, 0), new Point(0, -1), new Point(1, -1) };
            RightPos = new[] { new Point(0, 0), new Point(0, -1), new Point(1, 0), new Point(1, 1) };
            Deg180Pos = new[] { new Point(0, 0), new Point(1, 0), new Point(0, 1), new Point(-1, 1) };
            LeftPos = new[] { new Point(0, 0), new Point(-1, 0), new Point(-1, -1), new Point(0, 1) };
        }
    }
}