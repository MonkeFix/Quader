using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez.Persistence;

namespace Quader.Engine.RotationEncoder
{
    public class RotationEncoding
    {
        [JsonInclude]
        public PieceStartPosition StartPosition { get; set; }
        [JsonInclude]
        public string[] InitialEncoding { get; set; } = null!;
        [JsonInclude]
        public string[][] TestsClockwise { get; set; } = null!;
        [JsonInclude]
        public string[][] TestsCounterClockwise { get; set; } = null!;
    }

    public class RotationPositionEncoding
    {
        [JsonInclude]
        public Dictionary<PieceStartPosition, RotationEncoding> PositionEncodings { get; set; } = null!;
    }

    public class RotationSystemTable
    {
        [JsonInclude]
        public Dictionary<PieceType, RotationPositionEncoding> RotationSystemTableMap { get; set; } = null!;

        [JsonInclude]
        public ConverterOptions ConverterOptions { get; set; } = null!;
    }

    public class RotationSystemRowData
    {
        [JsonInclude]
        public Point Offset { get; set; }
        [JsonInclude]
        public int TestCount { get; set; }
    }
}