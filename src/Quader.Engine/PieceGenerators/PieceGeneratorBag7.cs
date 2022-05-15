using System;
using System.Collections.Generic;
using System.Linq;
using Nez;
using Quader.Engine.Pieces;

namespace Quader.Engine.PieceGenerators
{
    public class PieceGeneratorBag7 : IPieceGenerator
    {
        public int QueueSize { get; }

        public static List<PieceType> UsedPieceTypes { get; set; } = new()
        {
            PieceType.I, PieceType.O, PieceType.T, PieceType.J, PieceType.L, PieceType.Z, PieceType.S
        };

        private readonly Queue<PieceBase> _pieceQueue = new ();

        public PieceGeneratorBag7(int queueSize)
        {
            QueueSize = queueSize;
        }

        public IEnumerable<PieceBase> Initialize()
        {
            _pieceQueue.Clear();

            var bag = GenerateBag();
            var bag2 = GenerateBag();

            for (int i = QueueSize; i < 7; i++)
            {
                _pieceQueue.Enqueue(bag[i]);
            }

            EnqueueRange(bag2);

            return bag.Take(QueueSize).ToList();
        }

        public PieceBase Generate()
        {
            // Notes: It is guaranteed that a player won't get the same piece in a bag of 7 pieces
            //  That means a bag contains 7 unique pieces in random order
            //  Every bag goes back-to-back

            var p = _pieceQueue.Dequeue();

            if (_pieceQueue.Count <= QueueSize)
            {
                EnqueueRange(GenerateBag());
            }

            return p;
        }

        private void EnqueueRange(PieceBase[] data)
        {
            foreach (var p in data)
            {
                _pieceQueue.Enqueue(p);
            }
        }

        private PieceBase[] GenerateBag()
        {
            var pieceTypes = new PieceType[7];
            UsedPieceTypes.CopyTo(pieceTypes);

            pieceTypes.Shuffle();

            var pf = new PieceFactory();

            var result = new PieceBase[7];
            for (int i = 0; i < 7; i++)
            {
                result[i] = pf.Create(pieceTypes[i]);
            }

            return result;
        }
    }
}