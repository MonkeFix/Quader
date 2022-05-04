using System;
using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public class BoardCellContainer
    {
        public int Width { get; }
        public int Height { get; }
        
        private readonly BoardCellType[][] _board;
        
        public BoardCellContainer(int width, int height)
        {
            Width = width;
            Height = height;

            //_board = new BoardPieceType[Height, Width];
            _board = new BoardCellType[Height][];
            for (int i = 0; i < Height; i++)
            {
                _board[i] = new BoardCellType[Width];
            }
            
            Reset();
        }
        
        public void Reset()
        {
            ForEach((x, y) => _board[y][x] = BoardCellType.None);
        }
        
        public void MoveUp()
        {
            
        }

        public void MoveDown(int fromY = 0)
        {
            for (int y = fromY - 1; y >= 0; y--)
            {
                var empty = new BoardCellType[Width];
                var cur = _board[y];
                var tmp = new BoardCellType[Width];
                Array.Copy(cur, tmp, Width);

                _board[y] = empty;
                _board[y + 1] = tmp;
            }
        }
        
        public void ClearLine(int y)
        {
            for (int x = 0; x < Width; x++)
            {
                SetCellAt(x, y, BoardCellType.None);
            }
        }
        
        public bool IsLineFull(int y)
        {
            for (int i = 0; i < Width; i++)
            {
                if (GetCellAt(i, y) == BoardCellType.None)
                    return false;
            }

            return true;
        }

        public bool Intersects(Point[] points)
        {
            foreach (var point in points)
            {
                if (IsOutOfBounds(point) || GetCellAt(point.X, point.Y) != BoardCellType.None)
                    return true;
            }

            return false;
        }

        public bool IsOutOfBounds(Point p) => p.X < 0 || p.X >= Width || p.Y >= Height || p.Y < 0;
        
        public BoardCellType GetCellAt(int x, int y) => _board[y][x];
        public void SetCellAt(int x, int y, BoardCellType cell) => _board[y][x] = cell;
        
        
        private void ForEach(Action<int, int> action)
        {
            for (int y = 0; y < Height; y++)
            {
                for (int x = 0; x < Width; x++)
                {
                    action(x, y);
                }
            }
        }
    }
}