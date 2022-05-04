using System.Collections.Generic;
using Microsoft.Xna.Framework;

namespace Quader.Engine.Pieces.Impl
{
    public class PieceO : PieceBase
    {
        public override PieceType Type => PieceType.O;
        public override OffsetType OffsetType => OffsetType.BetweenCells;
        protected override Point[] SpawnPos { get; }
        protected override Point[] RightPos { get; }
        protected override Point[] Deg180Pos { get; }
        protected override Point[] LeftPos { get; }

        public override Dictionary<PieceRotationType, Point[]> WallKickData => PieceUtils.PieceOWallKickData;

        public PieceO()
        {
            SpawnPos = new[] { new Point(0, 0), new Point(-1, 0), new Point(0, -1), new Point(-1, -1) };
            RightPos = new[] { new Point(0, 0), new Point(-1, 0), new Point(0, -1), new Point(-1, -1) };
            Deg180Pos = new[] { new Point(0, 0), new Point(-1, 0), new Point(0, -1), new Point(-1, -1) };
            LeftPos = new[] { new Point(0, 0), new Point(-1, 0), new Point(0, -1), new Point(-1, -1) };
        }
    }
}