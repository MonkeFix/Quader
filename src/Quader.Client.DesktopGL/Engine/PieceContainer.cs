using System;
using System.Collections;
using System.Collections.Generic;
using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public class PieceContainer : IEnumerable<BoardCellType>
    {
        public int Width { get; }
        public int Height { get; }

        private readonly BoardCellType[][] _board;

        public bool UseBottomTopOrder { get; }
        
        
        public PieceContainer(int width, int height, bool useBottomTop = false)
        {
            Width = width;
            Height = height;
            UseBottomTopOrder = useBottomTop;
            
            _board = new BoardCellType[Height][];
            for (int i = 0; i < Height; i++)
            {
                _board[i] = new BoardCellType[Width];
            }
        }
        
        public bool Intersects(Point[] points)
        {
            foreach (var point in points)
            {
                if (point.Y < 0)
                    continue; // Skip tests upper than the board's border

                if (IsOutOfBounds(point) || GetPieceAt(point.X, point.Y) != BoardCellType.None)
                    return true;
            }

            return false;
        }

        public void Clear()
        {
            ForEach((x, y) => _board[y][x] = BoardCellType.None);
        }
        
        public void SetLineAt(int y, BoardCellType[] line, bool copy = false)
        {
            if (copy)
            {
                var tmp = new BoardCellType[line.Length];
                Array.Copy(line, tmp, line.Length);
                _board[y] = tmp;
            }
            else
                _board[y] = line;
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
                SetPieceAt(x, y, BoardCellType.None);
            }
        }
        
        public bool IsLineFull(int y)
        {
            for (int i = 0; i < Width; i++)
            {
                if (GetPieceAt(i, y) == BoardCellType.None)
                    return false;
            }

            return true;
        }

        public BoardCellType[] GetLineAt(int y) => _board[y];
        public BoardCellType GetPieceAt(int x, int y) => _board[y][x];
        public void SetPieceAt(int x, int y, BoardCellType cellType) => _board[y][x] = cellType;
        public bool IsOutOfBounds(Point p) => p.X < 0 || p.X >= Width || p.Y < 0 || p.Y >= Height;

        public IEnumerator<BoardCellType> GetEnumerator()
        {
            return new PieceContainerEnumerator(_board, Width, Height, UseBottomTopOrder);
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
        
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

    public class PieceContainerEnumerator : IEnumerator<BoardCellType>
    {
        public BoardCellType Current => _board[_y][_x];

        object IEnumerator.Current => Current;
        
        private int _curIndex = -1;
        private int _x = -1;
        private int _y = 0;
        private readonly int _size;

        private readonly bool _useBottomTopOrder;
        private readonly int _width;
        private readonly int _height;
        
        private readonly BoardCellType[][] _board;
        
        public PieceContainerEnumerator(BoardCellType[][] board, int width, int height, bool useBottomTop)
        {
            _board = board;
            _width = width;
            _height = height;
            _useBottomTopOrder = useBottomTop;
            
            if (_useBottomTopOrder)
                _y = _height - 1;

            _size = _width * _height;
        }
        
        public bool MoveNext()
        {
            if (_useBottomTopOrder)
            {
                if (_x + 1 == _width)
                {
                    _y--;
                    _x = 0;
                }
                else
                    _x++;
            }
            else
            {
                if (_x + 1 == _width)
                {
                    _y++;
                    _x = 0;
                }
                else
                    _x++;
            }
            
            _curIndex++;
            
            return _curIndex < _size;
        }

        public void Reset()
        {
            _curIndex = 0;
            _x = 0;
            
            if (_useBottomTopOrder)
                _y = _height - 1;
            else
                _y = 0;
        }

        public void Dispose() { }
    }
}