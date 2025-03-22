using Quader.Engine.Primitives;

namespace Quader.Engine.Pieces;

public class PiecePoints
{
    public static class PieceI
    {
        public static readonly Point[] InitPos =
        [
            new Point(-1, -1),
            new Point(-2, -1),
            new Point(1, -1),
            new Point(0, -1),
        ];

        public static readonly Point[] RightPos =
        [
            new Point(0, -1),
            new Point(0, 0),
            new Point(0, 1),
            new Point(0, -2)
        ];

        public static readonly Point[] Deg180Pos =
        [
            new Point(-1, 0),
            new Point(0, 0),
            new Point(1, 0),
            new Point(-2, 0),
        ];

        public static readonly Point[] LeftPos =
        [
            new Point(-1, -1),
            new Point(-1, -2),
            new Point(-1, 1),
            new Point(-1, 0),
        ];
    }

    public static class PieceO
    {
        public static readonly Point[] InitPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: -1, y: -1),
        ];

        public static readonly Point[] RightPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: -1, y: -1),
        ];

        public static readonly Point[] Deg180Pos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: -1, y: -1),
        ];

        public static readonly Point[] LeftPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: -1, y: -1),
        ];
    }

    public static class PieceT
    {
        public static readonly Point[] InitPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 1, y: 0),
            new Point(x: 0, y: -1),
        ];

        public static readonly Point[] RightPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 1, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: 0, y: 1),
        ];

        public static readonly Point[] Deg180Pos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 1, y: 0),
            new Point(x: 0, y: 1),
        ];

        public static readonly Point[] LeftPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: 0, y: 1),
        ];
    }

    public static class PieceL
    {
        public static readonly Point[] InitPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 1, y: 0),
            new Point(x: 1, y: -1),
        ];

        public static readonly Point[] RightPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: 0, y: 1),
            new Point(x: 1, y: 1),
        ];

        public static readonly Point[] Deg180Pos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: -1, y: 1),
            new Point(x: 1, y: 0),
        ];

        public static readonly Point[] LeftPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: -1, y: -1),
            new Point(x: 0, y: 1),
        ];
    }

    public static class PieceJ
    {
        public static readonly Point[] InitPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 1, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: -1, y: -1),
        ];

        public static readonly Point[] RightPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: 1, y: -1),
            new Point(x: 0, y: 1),
        ];

        public static readonly Point[] Deg180Pos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 1, y: 0),
            new Point(x: 1, y: 1),
        ];

        public static readonly Point[] LeftPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: 0, y: 1),
            new Point(x: -1, y: 1),
        ];
    }

    public static class PieceS
    {
        public static readonly Point[] InitPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: 1, y: -1),
        ];

        public static readonly Point[] RightPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: 1, y: 0),
            new Point(x: 1, y: 1),
        ];

        public static readonly Point[] Deg180Pos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 1, y: 0),
            new Point(x: 0, y: 1),
            new Point(x: -1, y: 1),
        ];

        public static readonly Point[] LeftPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: -1, y: -1),
            new Point(x: 0, y: 1),
        ];
    }

    public static class PieceZ
    {
        public static readonly Point[] InitPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: -1),
            new Point(x: 0, y: -1),
            new Point(x: 1, y: 0),
        ];

        public static readonly Point[] RightPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 0, y: 1),
            new Point(x: 1, y: 0),
            new Point(x: 1, y: -1),
        ];

        public static readonly Point[] Deg180Pos =
        [
            new Point(x: 0, y: 0),
            new Point(x: -1, y: 0),
            new Point(x: 0, y: 1),
            new Point(x: 1, y: 1),
        ];

        public static readonly Point[] LeftPos =
        [
            new Point(x: 0, y: 0),
            new Point(x: 0, y: -1),
            new Point(x: -1, y: 0),
            new Point(x: -1, y: 1),
        ];
    }
}