using System;
using Quader.Engine.Pieces.Impl;

namespace Quader.Engine.Pieces
{
    public class PieceFactory
    {
        public PieceBase Create(PieceType type)
        {
            return type switch
            {
                PieceType.I => new PieceI(),
                PieceType.O => new PieceO(),
                PieceType.T => new PieceT(),
                PieceType.L => new PieceL(), 
                PieceType.J => new PieceJ(),
                PieceType.S => new PieceS(),
                PieceType.Z => new PieceZ(),
                _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
            };
        }
    }
}