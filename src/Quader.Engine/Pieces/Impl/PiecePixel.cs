using System.Collections.Generic;
using System.Drawing;

namespace Quader.Engine.Pieces.Impl
{
    public class PiecePixel : PieceBase
    {
        public override PieceType Type => PieceType.Pixel;
        public override BoardCellType BoardCellType => BoardCellType.Garbage;
        protected override Point[] SpawnPos { get; } = new[] { new Point(0, 0) };
        protected override Point[] RightPos { get; } = new[] { new Point(0, 0) };
        protected override Point[] Deg180Pos { get; } = new[] { new Point(0, 0) };
        protected override Point[] LeftPos { get; } = new[] { new Point(0, 0) };

        public override Dictionary<PieceRotationType, Point[]> WallKickData => PieceUtils.PieceSettings!.PieceOWallKickData;
    }
}