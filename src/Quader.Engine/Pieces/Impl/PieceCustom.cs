using System.Collections.Generic;
using System.Drawing;

namespace Quader.Engine.Pieces.Impl
{
    public class PieceCustom : PieceBase
    {
        public override PieceType Type => PieceType.Custom;
        public override BoardCellType BoardCellType { get; }

        protected override Point[] SpawnPos { get; }
        protected override Point[] RightPos { get; }
        protected override Point[] Deg180Pos { get; }
        protected override Point[] LeftPos { get; }

        public override OffsetType OffsetType { get; }
        public override Dictionary<PieceRotationType, Point[]> WallKickData { get; }
        public override Color BaseColor { get; }


        public PieceCustom(
            Point[] spawn, Point[] right, Point[] deg180, Point[] left,
            OffsetType offsetType = OffsetType.Cell,
            Dictionary<PieceRotationType, Point[]>? wallKickData = null,
            Color? baseColor = null,
            BoardCellType cellType = BoardCellType.Garbage)
        {
            SpawnPos = spawn;
            RightPos = right;
            Deg180Pos = deg180;
            LeftPos = left;

            OffsetType = offsetType;
            BoardCellType = cellType;

            WallKickData = wallKickData ?? PieceUtils.PieceSettings!.DefaultWallKickData;
            BaseColor = baseColor ?? PieceUtils.PieceSettings!.ColorGarbage;
        }
    }
}