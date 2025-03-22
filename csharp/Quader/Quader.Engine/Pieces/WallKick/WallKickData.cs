using Quader.Engine.Primitives;

namespace Quader.Engine.Pieces.WallKick;

public class WallKickData
{
    public WallKickMode Mode { get; }

    public Dictionary<RotationMove, List<Point>> Default { get; }
    public Dictionary<RotationMove, List<Point>> PieceI { get; }
    public Dictionary<RotationMove, List<Point>> PieceO { get; }

    private static readonly List<Point> WkdToRight =
    [
        new Point(x: 0, y: 0),
        new Point(x: -1, y: 0),
        new Point(x: -1, y: 1),
        new Point(x: 0, y: -2),
        new Point(x: -1, y: -2),
    ];

    private static readonly List<Point> WkdToLeft =
    [
        new Point(x: 0, y: 0),
        new Point(x: 1, y: 0),
        new Point(x: 1, y: 1),
        new Point(x: 0, y: -2),
        new Point(x: 1, y: -2),
    ];

    private static readonly List<Point> WkdLeftToInitDeg180 =
    [
        new Point(x: 0, y: 0),
        new Point(x: -1, y: 0),
        new Point(x: -1, y: -1),
        new Point(x: 0, y: 2),
        new Point(x: -1, y: 2),
    ];

    private static readonly List<Point> WkdRightToInitDeg180 =
    [
        new Point(0, 0),
        new Point(1, 0),
        new Point(1, -1),
        new Point(0, 2),
        new Point(1, 2),
    ];

    private static readonly List<Point> WkdInitToDeg180 =
    [
        new Point(0, 0),
        new Point(0, 1),
        new Point(0, 2),
    ];

    private static readonly List<Point> WkdDeg180ToInit =
    [
        new Point(0, 0),
        new Point(0, -1),
        new Point(0, -2),
    ];

    public WallKickData(WallKickMode mode = WallKickMode.Standard)
    {
        Mode = mode;

        Default = new()
        {
            { RotationMove.InitToRight, WkdToRight },
            { RotationMove.RightToInit, WkdRightToInitDeg180 },
            { RotationMove.RightToDeg180, WkdRightToInitDeg180 },
            { RotationMove.Deg180ToRight, WkdToRight },
            { RotationMove.Deg180ToLeft, WkdToLeft },
            { RotationMove.LeftToDeg180, WkdLeftToInitDeg180 },
            { RotationMove.LeftToInit, WkdLeftToInitDeg180 },
            { RotationMove.InitToLeft, WkdToLeft },
            { RotationMove.InitToDeg180, WkdInitToDeg180 },
            { RotationMove.Deg180ToInit, WkdDeg180ToInit }
        };

        PieceI = new()
        {
            {
                RotationMove.InitToRight,
                [
                    new Point(0, 0),
                    new Point(-2, 0),
                    new Point(1, 0),
                    new Point(-2, -1),
                    new Point(1, 2),
                ]
            },
            {
                RotationMove.RightToInit,
                [
                    new Point(0, 0),
                    new Point(2, 0),
                    new Point(-1, 0),
                    new Point(2, 1),
                    new Point(-1, -2),
                ]
            },
            {
                RotationMove.RightToDeg180,
                [
                    new Point(0, 0),
                    new Point(-1, 0),
                    new Point(2, 0),
                    new Point(-1, 2),
                    new Point(2, -1),
                ]
            },
            {
                RotationMove.Deg180ToRight,
                [
                    new Point(0, 0),
                    new Point(1, 0),
                    new Point(-2, 0),
                    new Point(1, -2),
                    new Point(-2, 1),
                ]
            },
            {
                RotationMove.Deg180ToLeft,
                [
                    new Point(0, 0),
                    new Point(2, 0),
                    new Point(-1, 0),
                    new Point(2, 1),
                    new Point(-1, -2),
                ]
            },
            {
                RotationMove.LeftToDeg180,
                [
                    new Point(0, 0),
                    new Point(-2, 0),
                    new Point(1, 0),
                    new Point(-2, -1),
                    new Point(1, 2),
                ]
            },
            {
                RotationMove.LeftToInit,
                [
                    new Point(0, 0),
                    new Point(1, 0),
                    new Point(-2, 0),
                    new Point(1, -2),
                    new Point(-2, 1),
                ]
            },
            {
                RotationMove.InitToLeft,
                [
                    new Point(0, 0),
                    new Point(-1, 0),
                    new Point(2, 0),
                    new Point(-1, 2),
                    new Point(2, -1),
                ]
            },
            { RotationMove.InitToDeg180, WkdInitToDeg180 },
            { RotationMove.Deg180ToInit, WkdDeg180ToInit }
        };

        PieceO = new()
        {
            { RotationMove.InitToRight, [new Point(0, 0)] },
            { RotationMove.RightToInit, [new Point(0, 0)] },
            { RotationMove.RightToDeg180, [new Point(0, 0)] },
            { RotationMove.Deg180ToRight, [new Point(0, 0)] },
            { RotationMove.Deg180ToLeft, [new Point(0, 0)] },
            { RotationMove.LeftToDeg180, [new Point(0, 0)] },
            { RotationMove.LeftToInit, [new Point(0, 0)] },
            { RotationMove.InitToLeft, [new Point(0, 0)] },
            { RotationMove.InitToDeg180, [new Point(0, 0)] },
            { RotationMove.Deg180ToInit, [new Point(0, 0)] },
        };
    }

    public Dictionary<RotationMove, List<Point>> Get(WallKickType type)
    {
        return type switch
        {
            WallKickType.Default => Default,
            WallKickType.PieceI => PieceI,
            WallKickType.PieceO => PieceO,
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        };
    }
}