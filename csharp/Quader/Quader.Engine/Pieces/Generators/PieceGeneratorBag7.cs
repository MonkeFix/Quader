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

    public IEnumerable<PieceType> Initialize()
    {
        _queue.Clear();
        _queue = new Queue<PieceType>();

        var bag = GenerateBag();
        var bag2 = GenerateBag();

        var bagSize = AvailablePieces.Pieces.Length;
        for (int i = QueueSize; i < bagSize; i++)
        {
            _queue.Enqueue(bag[i]);
        }

        EnqueueRange(bag2);

        return bag.Take(QueueSize).ToList();
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
        var pieceTypes = new PieceType[7];
        AvailablePieces.Pieces.CopyTo(pieceTypes, 0);

        pieceTypes.Shuffle(_random);

        var result = new PieceType[7];
        for (int i = 0; i < 7; i++)
        {
            result[i] = pieceTypes[i];
        }

        return result;
    }

    private void EnqueueRange(IEnumerable<PieceType> types)
    {
        foreach (var type in types)
        {
            _queue.Enqueue(type);
        }
    }
}