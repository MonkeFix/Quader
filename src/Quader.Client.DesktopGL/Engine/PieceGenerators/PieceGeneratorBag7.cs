using System.Collections.Generic;
using Quader.Engine.Pieces;

namespace Quader.Engine.PieceGenerators
{
    public class PieceGeneratorBag7 : IPieceGenerator
    {
        public int QueueSize { get; }

        public PieceGeneratorBag7(int queueSize)
        {
            QueueSize = queueSize;
        }

        public IEnumerable<PieceBase> Initialize()
        {
            throw new System.NotImplementedException();
        }

        public PieceBase Generate()
        {
            throw new System.NotImplementedException();
        }
    }
}