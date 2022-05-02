using System;
using System.Collections.Generic;
using System.Linq;
using Random = Nez.Random;

namespace Quader.Engine.PieceGenerators
{
    public class PieceGeneratorFullRandom : IPieceGenerator
    {
        private readonly int _pieceCount = Enum.GetValues<PieceType>().Length;

        public int QueueSize { get; }

        public PieceGeneratorFullRandom(int queueSize = 4)
        {
            QueueSize = queueSize;
        }

        public IEnumerable<Piece> Initialize()
        {
            Piece[] queue = new Piece[QueueSize];

            for (int i = 0; i < QueueSize; i++)
            {
                queue[i] = new Piece(Rng(), null);
            }

            return queue;
        }

        public Piece Generate()
        {
            return new Piece(Rng(), null);
        }

        private PieceType Rng()
        {
            return (PieceType) Random.RNG.Next(0, _pieceCount);
        }
    }
}