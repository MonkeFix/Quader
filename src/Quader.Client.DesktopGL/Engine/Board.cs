using System;
using Microsoft.Xna.Framework;

namespace Quader.Engine
{
    public class Board
    {
        public static readonly int DefaultWidth = 10;
        public static readonly int DefaultHeight = 40;

        public int Width { get; }
        public int Height { get; }

        private BoardPieceType[] _board;

        public BoardPieceType[] BoardLayout => _board;

        private Piece? _currentPiece;

        public Piece? CurrentPiece => _currentPiece;

        public Board(int width = 10, int height = 20)
        {
            Width = width;
            Height = height;

            _board = new BoardPieceType[Width * Height];

            for (int i = 0; i < _board.Length; i++)
            {
                _board[i] = BoardPieceType.None;
            }
        }

        public void PushPiece(Piece piece)
        {
            _currentPiece = piece ?? throw new Exception("Invalid piece");

            //var height = _currentPiece.PieceTable.GetLength(0);
            var width = _currentPiece.PieceTable.GetLength(1);

            var initialX = Width / 2 - (int)Math.Round(width / 2.0);
            var initialY = 0;

            piece.X = initialX;
            piece.Y = initialY;

            /*for (var y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    if (_currentPiece.PieceTable[y, x])
                        SetPieceAt(initialX + x, initialY + y, (BoardPieceType)(int)piece.Type);
                }
            }*/
        }

        public void MoveLeft()
        {
            if (_currentPiece == null)
                return;

            if (_currentPiece.X > 0)
                _currentPiece.X -= 1;
        }

        public void MoveRight()
        {
            if (_currentPiece == null)
                return;

            if (_currentPiece.X + _currentPiece.PieceTable.GetLength(1) < Width)
                _currentPiece.X += 1;
        }

        public void HardDrop()
        {
            if (_currentPiece == null)
                return;


        }

        public void SoftDrop()
        {
            if (_currentPiece == null)
                return;

            _currentPiece.AbsoluteY += 1f;
        }

        public void Reset() => _board = new BoardPieceType[Width * Height];

        public void Update(float deltaTime)
        {
            if (_currentPiece == null)
                return;

            if (_currentPiece.Y < Height - 2)
            {
                _currentPiece.AbsoluteY += 1f * deltaTime;
                _currentPiece.Y = (int)Math.Floor(_currentPiece.AbsoluteY);
            }

            /*
            var width = _currentPiece.PieceTable.GetLength(0);
            var height = _currentPiece.PieceTable.GetLength(1);

            for (var y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    if (_currentPiece.PieceTable[y, x])
                        SetPieceAt(initialX + x, initialY + y, (BoardPieceType)(int)piece.Type);
                }
            }*/

            // TODO: Check line clears
        }

        public BoardPieceType GetPieceAt(int x, int y) => _board[GetIndexByCoordinates(x, y)];
        public void SetPieceAt(int x, int y, BoardPieceType piece) => _board[GetIndexByCoordinates(x, y)] = piece;
        public int GetIndexByCoordinates(int x, int y) => x + Width * y;
        public int GetIndexByCoordinates(Point coordinates) => GetIndexByCoordinates(coordinates.X, coordinates.Y);
        public Point GetCoordinatesByIndex(int index) => new Point(index / Width, index / Height);
    }
}