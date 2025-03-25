using System.Numerics;
using Quader.Engine.Boards;
using Quader.Engine.Primitives;
using Quader.Engine.Scoring;

namespace Quader.Engine.Pieces;

public static class PieceHelpers
{
    public static Point[] GetPointsForPiece(PieceType pieceType, RotationState state)
    {
        switch (pieceType)
        {
            case PieceType.I:
                switch (state)
                {
                    case RotationState.Initial:
                        return PiecePoints.PieceI.InitPos;
                    case RotationState.Clockwise:
                        return PiecePoints.PieceI.RightPos;
                    case RotationState.Deg180:
                        return PiecePoints.PieceI.Deg180Pos;
                    case RotationState.CounterClockwise:
                        return PiecePoints.PieceI.LeftPos;
                }

                break;
            case PieceType.O:
                switch (state)
                {
                    case RotationState.Initial:
                        return PiecePoints.PieceO.InitPos;
                    case RotationState.Clockwise:
                        return PiecePoints.PieceO.RightPos;
                    case RotationState.Deg180:
                        return PiecePoints.PieceO.Deg180Pos;
                    case RotationState.CounterClockwise:
                        return PiecePoints.PieceO.LeftPos;
                }

                break;
            case PieceType.T:
                switch (state)
                {
                    case RotationState.Initial:
                        return PiecePoints.PieceT.InitPos;
                    case RotationState.Clockwise:
                        return PiecePoints.PieceT.RightPos;
                    case RotationState.Deg180:
                        return PiecePoints.PieceT.Deg180Pos;
                    case RotationState.CounterClockwise:
                        return PiecePoints.PieceT.LeftPos;
                }

                break;
            case PieceType.L:
                switch (state)
                {
                    case RotationState.Initial:
                        return PiecePoints.PieceL.InitPos;
                    case RotationState.Clockwise:
                        return PiecePoints.PieceL.RightPos;
                    case RotationState.Deg180:
                        return PiecePoints.PieceL.Deg180Pos;
                    case RotationState.CounterClockwise:
                        return PiecePoints.PieceL.LeftPos;
                }

                break;
            case PieceType.J:
                switch (state)
                {
                    case RotationState.Initial:
                        return PiecePoints.PieceJ.InitPos;
                    case RotationState.Clockwise:
                        return PiecePoints.PieceJ.RightPos;
                    case RotationState.Deg180:
                        return PiecePoints.PieceJ.Deg180Pos;
                    case RotationState.CounterClockwise:
                        return PiecePoints.PieceJ.LeftPos;
                }

                break;
            case PieceType.S:
                switch (state)
                {
                    case RotationState.Initial:
                        return PiecePoints.PieceS.InitPos;
                    case RotationState.Clockwise:
                        return PiecePoints.PieceS.RightPos;
                    case RotationState.Deg180:
                        return PiecePoints.PieceS.Deg180Pos;
                    case RotationState.CounterClockwise:
                        return PiecePoints.PieceS.LeftPos;
                }

                break;
            case PieceType.Z:
                switch (state)
                {
                    case RotationState.Initial:
                        return PiecePoints.PieceZ.InitPos;
                    case RotationState.Clockwise:
                        return PiecePoints.PieceZ.RightPos;
                    case RotationState.Deg180:
                        return PiecePoints.PieceZ.Deg180Pos;
                    case RotationState.CounterClockwise:
                        return PiecePoints.PieceZ.LeftPos;
                }

                break;
            case PieceType.Pixel:
            default:
                throw new ArgumentOutOfRangeException(nameof(pieceType), pieceType,
                    "Invalid PieceType");
        }

        throw new ArgumentOutOfRangeException(nameof(pieceType), pieceType,
            "Invalid PieceType");
    }

    public static OffsetType PieceTypeToOffsetType(PieceType type)
        => type switch
        {
            PieceType.I or PieceType.O => OffsetType.BetweenCells,
            _ => OffsetType.Cell
        };

    public static CellType PieceTypeToCellType(PieceType type) =>
        type switch
        {
            PieceType.I => CellType.I,
            PieceType.O => CellType.O,
            PieceType.T => CellType.T,
            PieceType.L => CellType.L,
            PieceType.J => CellType.J,
            PieceType.S => CellType.S,
            PieceType.Z => CellType.Z,
            PieceType.Pixel => CellType.Garbage,
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        };

    public static Rectangle CalcBounds(Point[] positions, int x, int y)
    {
        var minX = int.MaxValue;
        var minY = int.MaxValue;
        var maxX = int.MinValue;
        var maxY = int.MinValue;

        foreach (var pos in positions)
        {
            if (pos.X < minX) minX = pos.X;
            else if (pos.X > maxX) maxX = pos.X;

            if (pos.Y < minY) minY = pos.Y;
            else if (pos.Y > maxY) maxY = pos.Y;
        }

        var w = 1 + (minX == maxX ? 0 : Math.Abs(minX) + Math.Abs(maxX));
        var h = 1 + (minY == maxY ? 0 : Math.Abs(minY) + Math.Abs(maxY));

        return new Rectangle(
            x + minX, y + minY, w, h
        );
    }

    public static Color PieceTypeToColor(PieceType type) =>
        type switch
        {
            PieceType.I => Color.PieceI,
            PieceType.O => Color.PieceO,
            PieceType.T => Color.PieceT,
            PieceType.L => Color.PieceL,
            PieceType.J => Color.PieceJ,
            PieceType.S => Color.PieceS,
            PieceType.Z => Color.PieceZ,
            PieceType.Pixel => Color.PieceGarbage,
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        };

    public static List<Point> AdjustPositions(IEnumerable<Point> points, Point offset)
    {
        var res = new List<Point>();
        foreach (var point in points)
        {
            res.Add(new Point(point.X + offset.X, point.Y + offset.Y));
        }

        return res;
    }

    public static bool IsOob(int x, int y, int width, int height)
    {
        return x < 0 || x >= width || y >= height || y < 0;
    }

    public static TSpinStatus TestTOverhang(BoardSettings boardSettings, int pieceX, int pieceY,
        Func<Point, bool> notEmptyFunc)
    {
        Point[] pointArr =
        [
            new Point(pieceX - 1, pieceY - 1),
            new Point(pieceX + 1, pieceY - 1),
            new Point(pieceX - 1, pieceY + 1),
            new Point(pieceX + 1, pieceY + 1)
        ];

        var oobOverhangs = 0;
        var nonOobOverhangs = 0;

        foreach (var p in pointArr)
        {
            if (IsOob(p.X, p.Y, boardSettings.Width, boardSettings.Height))
                oobOverhangs++;
            else if (notEmptyFunc(p))
                nonOobOverhangs++;
        }

        if (oobOverhangs > 0 && nonOobOverhangs > 0)
            return TSpinStatus.Mini;

        if (oobOverhangs == 0 && nonOobOverhangs >= 3)
            return TSpinStatus.Full;

        return TSpinStatus.None;
    }
}