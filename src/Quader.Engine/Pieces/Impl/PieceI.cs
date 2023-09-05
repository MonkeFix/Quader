using System.Collections.Generic;
using System.Drawing;

namespace Quader.Engine.Pieces.Impl
{
    public class PieceI : PieceBase
    {
        public override PieceType Type => PieceType.I;
        public override BoardCellType BoardCellType => BoardCellType.I;
        public override OffsetType OffsetType => OffsetType.BetweenCells;
        protected override Point[] SpawnPos { get; } = new[] { new Point(-1, -1), new Point(-2, -1), new Point(1, -1), new Point(0, -1) };
        protected override Point[] RightPos { get; } = new[] { new Point(0, -1), new Point(0, 0), new Point(0, 1), new Point(0, -2) };
        protected override Point[] Deg180Pos { get; } = new[] { new Point(-1, 0), new Point(0, 0), new Point(1, 0), new Point(-2, 0) };
        protected override Point[] LeftPos { get; } = new[] { new Point(-1, -1), new Point(-1, -2), new Point(-1, 1), new Point(-1, 0) };

        public override Dictionary<PieceRotationType, Point[]> WallKickData => PieceSettings.PieceIWallKickData;
    }
}
