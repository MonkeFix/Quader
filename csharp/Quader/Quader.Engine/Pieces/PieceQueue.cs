using System.Collections;
using Quader.Engine.Pieces.Generators;

namespace Quader.Engine.Pieces;

public class PieceQueue : IEnumerable<PieceType>
{
    private Queue<PieceType> _queue;
    private IPieceGenerator _pieceGenerator;
    private PieceType _nextPiece;
    private int _seed;

    public PieceQueue(int seed)
    {
        _seed = seed;
        _pieceGenerator = new PieceGeneratorBag7(_seed);
        _nextPiece = PieceType.Pixel;

        var q = _pieceGenerator.Initialize();
        _queue = new Queue<PieceType>();
        foreach (var pieceType in q)
            _queue.Enqueue(pieceType);
    }

    public PieceType NextPiece() => SetPiece();

    public void Reset(int? newSeed)
    {
        if (newSeed.HasValue) _seed = newSeed.Value;

        var pieceGenerator = new PieceGeneratorBag7(_seed);
        var queue = pieceGenerator.Initialize();

        _queue = new Queue<PieceType>();
        foreach (var pieceType in queue)
            _queue.Enqueue(pieceType);

        _pieceGenerator = pieceGenerator;
        _nextPiece = PieceType.Pixel;
    }

    private PieceType SetPiece()
    {
        _nextPiece = _pieceGenerator.Next();
        var next = _queue.Dequeue();

        _queue.Enqueue(_nextPiece);

        return next;
    }

    public IEnumerator<PieceType> GetEnumerator()
    {
        return _queue.GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}