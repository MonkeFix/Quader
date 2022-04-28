using System.Collections.Generic;

namespace Quader.Engine.PieceGenerators
{
    public interface IPieceGenerator
    {
        int QueueSize { get; }

        IEnumerable<Piece> Initialize(); 
        Piece Generate();
    }
}