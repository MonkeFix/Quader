namespace Quader.Engine.Pieces.Generators;

public class PieceGeneratorFullRandom : IPieceGenerator
{
    public int QueueSize => 5;

    private Random _random;

    public PieceGeneratorFullRandom(int seed)
    {
        _random = new Random(seed);
    }

    public IEnumerable<PieceType> Initialize()
    {
        var result = new Queue<PieceType>();

        for (int i = 0; i < QueueSize; i++)
        {
            result.Enqueue(Rng());
        }

        return result;
    }

    public PieceType Next() => Rng();

    private PieceType Rng()
    {
        var i = _random.Next(0, AvailablePieces.Pieces.Length);
        return AvailablePieces.Pieces[i];
    }
}