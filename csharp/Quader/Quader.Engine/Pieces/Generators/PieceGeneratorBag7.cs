using Quader.Engine.Extensions;

namespace Quader.Engine.Pieces.Generators;

public class PieceGeneratorBag7 : IPieceGenerator
{
    public int QueueSize => 5;

    private Random _random;
    private Queue<PieceType> _queue = new();

    public PieceGeneratorBag7(int seed)
    {
        _random = new Random(seed);
    }

    public Queue<PieceType> Initialize()
    {
        var bag1 = GenerateBag();
        var bag2 = GenerateBag();

        var bagSize = AvailablePieces.Pieces.Length;
        for (int i = QueueSize; i < bagSize; i++)
        {
            _queue.Enqueue(bag1[i]);
        }

        EnqueueRange(bag2);

        return _queue;
    }

    public PieceType Next()
    {
        var p = _queue.Dequeue();

        if (_queue.Count <= QueueSize)
        {
            var bag = GenerateBag();
            EnqueueRange(bag);
        }

        return p;
    }

    private PieceType[] GenerateBag()
    {
        var len = AvailablePieces.Pieces.Length;
        var dest = new PieceType[len];
        Array.Copy(AvailablePieces.Pieces, dest, len);

        dest.Shuffle(_random);
        return dest;
    }

    private void EnqueueRange(IEnumerable<PieceType> types)
    {
        foreach (var type in types)
        {
            _queue.Enqueue(type);
        }
    }
}