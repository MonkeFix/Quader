using System;
using System.Transactions;
using Microsoft.Xna.Framework;
using Quader.Engine.RotationEncoder;

namespace Quader.Engine
{
    public class Board
    {
        public static readonly int DefaultWidth = 10;
        public static readonly int DefaultHeight = 20;

        public int Width { get; }
        public int Height { get; }

        private BoardPieceType[] _board;

        public BoardPieceType[] BoardLayout => _board;

        private Piece? _currentPiece;
        private Piece? _heldPiece;

        public Piece? CurrentPiece => _currentPiece;
        public Piece? HeldPiece => _heldPiece;

        public Board()
            : this(DefaultWidth, DefaultHeight)
        { }

        public Board(int width, int height)
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

            var width = _currentPiece.Width;

            var initialX = Width / 2 - (int)Math.Round(width / 2.0);
            var initialY = -1;

            piece.X = initialX;
            piece.AbsoluteY = initialY;
            piece.Y = initialY;
        }

        public void Hold(Piece newPiece)
        {
            _heldPiece = _currentPiece;
            PushPiece(newPiece);
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

            if (_currentPiece.X + _currentPiece.Width < Width)
                _currentPiece.X += 1;
        }

        public void HardDrop(Piece newPiece)
        {
            if (_currentPiece == null)
                return;

            var nearestY = FindNearestDropY();

            var width = _currentPiece.Width;
            var height = _currentPiece.Height;

            var initialX = _currentPiece.X;
            var initialY = nearestY;

            for (var y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    if (_currentPiece.PieceTable[y, x])
                        SetPieceAt(initialX + x, initialY + y, (BoardPieceType)(int)_currentPiece.Type);
                }
            }

            PushPiece(newPiece);
        }

        public void SoftDrop()
        {
            if (_currentPiece == null)
                return;

            _currentPiece.AbsoluteY += 1f;
        }

        public void RotateClockwise()
        {
            if (_currentPiece == null)
                return;

            _currentPiece.RotateClockwise();
        }

        public void RotateCounterClockwise()
        {
            if (_currentPiece == null)
                return;

            _currentPiece.RotateCounterClockwise();
        }

        public void Rotate180()
        {
            if (_currentPiece == null)
                return;

            _currentPiece.Rotate180();
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
        }

        public int FindNearestDropY()
        {
            if (_currentPiece == null)
                return 0;

            var dropY = Math.Max(_currentPiece.Y, 0);

            for (int y = dropY; y <= Height - _currentPiece.Height; y++)
            {
                if (!TestPositionY(y))
                    break;

                dropY = y;
            }

            return dropY;
        }

        private bool TestPositionY(int y)
        {
            if (_currentPiece == null)
                return false;
            
            return TestPosition(_currentPiece.X, y);
        }

        private bool TestPosition(int y)
        {
            if (_currentPiece == null)
                return false;

            return TestPosition(_currentPiece.Table, _currentPiece.X, y);
        }

        private bool TestPosition(string[] data, int x, int y)
        {
            if (data.Length == 0)
                return false;
            
            var w = data[0].Length;
            var h = data.Length;

            // TODO: there must be a much faster algorithm
            for (int i = 0; i < h; i++)
            {
                for (int j = 0; j < w; j++)
                {
                    var p = data[i][j];
                    if (p != ConverterOptions.FilledChar)
                        continue;
                    
                    var oX = x + j;
                    var oY = y + i;
                    
                    // we can't rotate because of the walls
                    if (oX < 0 || oX >= Width || oY >= Height)
                        return false;
                    
                    // just skip if upper that the field's start Y
                    if (oY < 0) continue;

                    if (GetPieceAt(oX, oY) != BoardPieceType.None)
                        return false;
                }
            }

            return true;
        }

        private bool TestAll(string[][] tests, int x, int y, out string[]? firstPassedTest, out int? firstPassedTestIndex)
        {
            for (int i = 0; i < tests.Length; i++)
            {
                var test = tests[i];
                var res = TestPosition(test, x, y);
                if (res)
                {
                    firstPassedTest = test;
                    firstPassedTestIndex = i;
                    return true;
                }
                else
                    break;
            }

            firstPassedTest = null;
            firstPassedTestIndex = null;
            return false;
        }
        
        private bool TestPosition(int x, int y)
        {
            if (_currentPiece == null)
                return false;

            var width = _currentPiece.Width;
            var height = _currentPiece.Height;

            for (int i = 0; i < height; i++)
            {
                for (int j = 0; j < width; j++)
                {
                    if (_currentPiece.PieceTable[i, j] && GetPieceAt(x + j, y + i) != BoardPieceType.None)
                        return false;
                }
            }
            
            return true;
        }

        public BoardPieceType GetPieceAt(int x, int y) => _board[GetIndexByCoordinates(x, y)];
        public void SetPieceAt(int x, int y, BoardPieceType piece) => _board[GetIndexByCoordinates(x, y)] = piece;
        public int GetIndexByCoordinates(int x, int y) => x + Width * y;
        public int GetIndexByCoordinates(Point coordinates) => GetIndexByCoordinates(coordinates.X, coordinates.Y);
        public Point GetCoordinatesByIndex(int index) => new Point(index / Width, index / Height);
    }
}