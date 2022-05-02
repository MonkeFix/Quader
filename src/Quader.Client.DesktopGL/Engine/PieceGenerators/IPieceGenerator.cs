using System.Collections.Generic;
using Quader.Engine.Pieces;

namespace Quader.Engine.PieceGenerators
{
    public interface IPieceGenerator
    {
        int QueueSize { get; }

        IEnumerable<PieceBase> Initialize(); 
        PieceBase Generate();
    }
}