namespace Quader.Engine.Pieces.Generators;

public static class AvailablePieces
{
    public static readonly PieceType[] Pieces =
    [
        PieceType.S, PieceType.Z, PieceType.L, PieceType.O, PieceType.J, PieceType.I, PieceType.T
    ];
}

public interface IPieceGenerator
{
    int QueueSize { get; }
    Queue<PieceType> Initialize();
    PieceType Next();
}