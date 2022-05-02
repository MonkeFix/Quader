﻿using System.Collections.Generic;
using Microsoft.Xna.Framework;

namespace Quader.Engine.Pieces.Impl
{
    public class PieceI : PieceBase
    {
        public override PieceType Type => PieceType.I;
        public override OffsetType OffsetType => OffsetType.BetweenCells;
        public override Point[] SpawnPos { get; }
        public override Point[] RightPos { get; }
        public override Point[] Deg180Pos { get; }
        public override Point[] LeftPos { get; }

        public override Dictionary<PieceRotationType, Point[]> WallKickData => PieceUtils.PieceIWallKickData;

        public PieceI()
        {
            Deg180Pos = new[] { new Point(-1, 0), new Point(0, 0), new Point(1, 0), new Point(-2, 0) };
            SpawnPos = new[] { new Point(-1, -1), new Point(-2, -1), new Point(1, -1), new Point(0, -1) };
            
            LeftPos = new[] { new Point(-1, -1), new Point(-1, -2), new Point(-1, 1), new Point(-1, 0) };
            RightPos = new[] { new Point(0, -1), new Point(0, 0), new Point(0, 1), new Point(0, -2) };
        }
    }
}
