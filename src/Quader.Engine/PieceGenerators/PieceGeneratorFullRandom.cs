using System;
using System.Collections.Generic;
using System.Linq;
using Quader.Engine.Pieces;
using Random = Nez.Random;

namespace Quader.Engine.PieceGenerators
{
    public class PieceGeneratorFullRandom : IPieceGenerator
    {
        private readonly int _pieceCount = Enum.GetValues<PieceType>().Length;

        private readonly PieceFactory _pf = new PieceFactory();
        
        public int QueueSize { get; }

        public PieceGeneratorFullRandom(int queueSize = 4)
        {
            QueueSize = queueSize;
        }

        public IEnumerable<PieceBase> Initialize()
        {
            PieceBase[] queue = new PieceBase[QueueSize];

            for (int i = 0; i < QueueSize; i++)
            {
                queue[i] = _pf.Create(Rng());
            }

            return queue;
        }

        public PieceBase Generate()
        {
            return _pf.Create(Rng());
        }

        private PieceType Rng()
        {
            return (PieceType) Random.RNG.Next(0, _pieceCount);
        }
    }
}