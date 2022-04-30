using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez.Persistence;

namespace Quader.Engine.RotationEncoder
{
    public class ConverterOptions
    {
        [JsonInclude]
        public int SegmentSize { get; set; }
        [JsonInclude]
        public Dictionary<PieceType, RotationSystemRowData>? RowDataSettings { get; set; } = null;

        public static ConverterOptions Default => new ConverterOptions
        {
            SegmentSize = 16,
            RowDataSettings = new()
            {
                { PieceType.I, new RotationSystemRowData { Offset = new Point(8, 8), TestCount = 5 } },
                { PieceType.O, new RotationSystemRowData { Offset = new Point(14, 14), TestCount = 5 } },
                { PieceType.T, new RotationSystemRowData { Offset = new Point(10, 10), TestCount = 5 } },
                { PieceType.L, new RotationSystemRowData { Offset = new Point(10, 10), TestCount = 5 } },
                { PieceType.J, new RotationSystemRowData { Offset = new Point(10, 10), TestCount = 5 } },
                { PieceType.S, new RotationSystemRowData { Offset = new Point(10, 10), TestCount = 5 } },
                { PieceType.Z, new RotationSystemRowData { Offset = new Point(10, 10), TestCount = 5 } },
            }
        };
    }
}