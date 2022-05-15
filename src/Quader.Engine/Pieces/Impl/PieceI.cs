using System.Collections.Generic;
using Microsoft.Xna.Framework;

namespace Quader.Engine.Pieces.Impl
{
    public class PieceI : PieceBase
    {
        public override PieceType Type => PieceType.I;
        public override BoardCellType BoardCellType => BoardCellType.I;
        public override OffsetType OffsetType => OffsetType.BetweenCells;
        protected override Point[] SpawnPos { get; }
        protected override Point[] RightPos { get; }
        protected override Point[] Deg180Pos { get; }
        protected override Point[] LeftPos { get; }

        public override Dictionary<PieceRotationType, Point[]> WallKickData => PieceUtils.PieceSettings!.PieceIWallKickData;

        public PieceI()
        {
            Deg180Pos = new[] { new Point(-1, 0), new Point(0, 0), new Point(1, 0), new Point(-2, 0) };
            SpawnPos = new[] { new Point(-1, -1), new Point(-2, -1), new Point(1, -1), new Point(0, -1) };
            
            LeftPos = new[] { new Point(-1, -1), new Point(-1, -2), new Point(-1, 1), new Point(-1, 0) };
            RightPos = new[] { new Point(0, -1), new Point(0, 0), new Point(0, 1), new Point(0, -2) };
        }
    }
}
