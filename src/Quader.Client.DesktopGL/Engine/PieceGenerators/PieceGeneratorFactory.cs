using System;

namespace Quader.Engine.PieceGenerators
{
    public class PieceGeneratorFactory
    {
        public int QueueSize { get; }

        public PieceGeneratorFactory(int queueSize)
        {
            QueueSize = queueSize;
        }

        public IPieceGenerator Create(PieceGeneratorType type)
        {
            switch (type)
            {
                case PieceGeneratorType.FullRandom:
                    return new PieceGeneratorFullRandom(QueueSize);
                case PieceGeneratorType.Bag7:
                    return new PieceGeneratorBag7(QueueSize);
                default:
                    throw new ArgumentOutOfRangeException(nameof(type), type, null);
            }
        }
    }
}