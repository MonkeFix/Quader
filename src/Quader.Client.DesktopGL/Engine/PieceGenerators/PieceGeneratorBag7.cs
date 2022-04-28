using System.Collections.Generic;

namespace Quader.Engine.PieceGenerators
{
    public class PieceGeneratorBag7 : IPieceGenerator
    {
        public int QueueSize { get; }

        public PieceGeneratorBag7(int queueSize)
        {
            QueueSize = queueSize;
        }

        public IEnumerable<Piece> Initialize()
        {
            throw new System.NotImplementedException();
        }

        public Piece Generate()
        {
            throw new System.NotImplementedException();
        }
    }
}