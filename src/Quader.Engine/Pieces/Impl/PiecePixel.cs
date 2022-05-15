using System.Collections.Generic;
using Microsoft.Xna.Framework;

namespace Quader.Engine.Pieces.Impl
{
    public class PiecePixel : PieceBase
    {
        public override PieceType Type => PieceType.Pixel;
        public override BoardCellType BoardCellType => BoardCellType.Garbage;
        protected override Point[] SpawnPos { get; }
        protected override Point[] RightPos { get; }
        protected override Point[] Deg180Pos { get; }
        protected override Point[] LeftPos { get; }

        public override Dictionary<PieceRotationType, Point[]> WallKickData => PieceUtils.PieceSettings!.PieceOWallKickData;
        
        public PiecePixel()
        {
            SpawnPos = new[] { new Point(0, 0) };
            RightPos = new[] { new Point(0, 0) };
            Deg180Pos = new[] { new Point(0, 0) };
            LeftPos = new[] { new Point(0, 0) };
        }
    }
}