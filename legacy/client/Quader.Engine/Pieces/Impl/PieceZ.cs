using System.Drawing;

namespace Quader.Engine.Pieces.Impl
{
    public class PieceZ : PieceBase
    {
        public override PieceType Type => PieceType.Z;
        public override BoardCellType BoardCellType => BoardCellType.Z;
        protected override Point[] SpawnPos { get; } = new[] { new Point(0,0), new Point(-1, -1), new Point(0, -1), new Point(1, 0)};
        protected override Point[] RightPos { get; } = new[] { new Point(0, 0), new Point(0, 1), new Point(1, 0), new Point(1, -1) };
        protected override Point[] Deg180Pos { get; } = new[] { new Point(0, 0), new Point(-1, 0), new Point(0, 1), new Point(1, 1) };
        protected override Point[] LeftPos { get; } = new[] { new Point(0,0), new Point(0, -1), new Point(-1, 0), new Point(-1, 1) };
    }
}