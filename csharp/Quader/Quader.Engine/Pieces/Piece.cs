using Quader.Engine.Boards;
using Quader.Engine.Pieces.WallKick;
using Quader.Engine.Primitives;

namespace Quader.Engine.Pieces;

public readonly struct WallKickCheckParams(IEnumerable<Point> tests, IEnumerable<Point> expectedPos)
{
    public readonly IEnumerable<Point> Tests = tests;
    public readonly IEnumerable<Point> ExpectedPos = expectedPos;
}

public class Piece
{
    public PieceType PieceType { get; }
    public CellType CellType { get; }
    public OffsetType OffsetType { get; }
    public Rectangle Bounds { get; private set; }

    private int _x;

    public int X
    {
        get => _x;
        set => SetX(value);
    }

    private int _y;

    public int Y
    {
        get => _y;
        set => SetY(value);
    }

    public RotationState CurrentRotation { get; private set; }
    public WallKickType WallKickType { get; }

    public Color Color => PieceHelpers.PieceTypeToColor(PieceType);

    public Piece(PieceType type)
    {
        PieceType = type;
        CurrentRotation = RotationState.Initial;
        _x = 0;
        _y = 0;

        WallKickType = PieceType switch
        {
            PieceType.I => WallKickType.PieceI,
            PieceType.O => WallKickType.PieceO,
            _ => WallKickType.Default
        };

        OffsetType = PieceHelpers.PieceTypeToOffsetType(PieceType);
        CellType = PieceHelpers.PieceTypeToCellType(PieceType);

        var initPos = GetPoints();
        Bounds = PieceHelpers.CalcBounds(initPos, X, Y);
    }

    public void Reset()
    {
        CurrentRotation = RotationState.Initial;
        Bounds = CalcBounds();
    }

    public Point[] GetPoints() => PieceHelpers.GetPointsForPiece(PieceType, CurrentRotation);
    public Point[] GetPositions() => GetPoints();

    public void SetX(int x)
    {
        _x = x;
        Bounds = CalcBounds();
    }

    public void SetY(int y)
    {
        _y = y;
        Bounds = CalcBounds();
    }

    public void MoveLeft() => X -= 1;
    public void MoveRight() => X += 1;
    public void MoveDown() => Y += 1;

    public void Rotate(RotationDirection rotation, int xOffset, int yOffset)
    {
        RotateSimple(rotation);

        var ix = X + xOffset;
        var iy = Y + yOffset;

        _x = ix;
        _y = iy;

        Bounds = CalcBounds();
    }

    public (RotationMove Move, Point[] Points) GetRotationType(RotationDirection rotation)
    {
        return CurrentRotation switch
        {
            RotationState.Initial => rotation switch
            {
                RotationDirection.Clockwise => (RotationMove.InitToRight,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Clockwise)),
                RotationDirection.CounterClockwise => (RotationMove.InitToLeft,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.CounterClockwise)),
                RotationDirection.Deg180 => (RotationMove.InitToDeg180,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Deg180)),
                _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
            },
            RotationState.Clockwise => rotation switch
            {
                RotationDirection.Clockwise => (RotationMove.RightToDeg180,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Deg180)),
                RotationDirection.CounterClockwise => (RotationMove.RightToInit,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Initial)),
                RotationDirection.Deg180 => (RotationMove.Deg180ToLeft,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.CounterClockwise)),
                _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
            },
            RotationState.Deg180 => rotation switch
            {
                RotationDirection.Clockwise => (RotationMove.Deg180ToLeft,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.CounterClockwise)),
                RotationDirection.CounterClockwise => (RotationMove.Deg180ToRight,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Clockwise)),
                RotationDirection.Deg180 => (RotationMove.Deg180ToInit,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Initial)),
                _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
            },
            RotationState.CounterClockwise => rotation switch
            {
                RotationDirection.Clockwise => (RotationMove.LeftToInit,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Initial)),
                RotationDirection.CounterClockwise => (RotationMove.LeftToDeg180,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Deg180)),
                RotationDirection.Deg180 => (RotationMove.InitToRight,
                    PieceHelpers.GetPointsForPiece(PieceType, RotationState.Clockwise)),
                _ => throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null)
            },
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    internal void RotateSimple(RotationDirection rotation)
    {
        switch (rotation)
        {
            case RotationDirection.Clockwise:
                RotateRight();
                break;
            case RotationDirection.CounterClockwise:
                RotateLeft();
                break;
            case RotationDirection.Deg180:
                RotateRight();
                RotateRight();
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
        }

        Bounds = CalcBounds();
    }

    void RotateRight()
    {
        CurrentRotation = CurrentRotation switch
        {
            RotationState.Initial => RotationState.Clockwise,
            RotationState.Clockwise => RotationState.Deg180,
            RotationState.Deg180 => RotationState.CounterClockwise,
            RotationState.CounterClockwise => RotationState.Initial,
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    void RotateLeft()
    {
        CurrentRotation = CurrentRotation switch
        {
            RotationState.Initial => RotationState.CounterClockwise,
            RotationState.Clockwise => RotationState.Initial,
            RotationState.Deg180 => RotationState.Clockwise,
            RotationState.CounterClockwise => RotationState.Deg180,
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private Rectangle CalcBounds() => PieceHelpers.CalcBounds(GetPoints(), X, Y);
}