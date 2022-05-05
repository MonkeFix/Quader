using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez.Persistence;
using Quader.Engine.Pieces;

namespace Quader.Engine.RotationEncoder
{
    public class RotationTableTests
    {
        [JsonInclude] public Dictionary<PieceRotationType, Point[]> DefaultWallKickData { get; set; }

        [JsonInclude] public Dictionary<PieceRotationType, Point[]> PieceIWallKickData { get; set; }

        [JsonInclude] public Dictionary<PieceRotationType, Point[]> PieceOWallKickData { get; set; }
    }
}