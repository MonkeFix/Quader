using System;
using System.Transactions;
using Microsoft.Xna.Framework;
using Quader.Engine.RotationEncoder;

namespace Quader.Engine
{
    public class BoardOld
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
        
        public RotationSystemTable RotationSystemTable { get; }

        public BoardOld(RotationSystemTable rotationSystemTable)
            : this(DefaultWidth, DefaultHeight, rotationSystemTable)
        { }

        public BoardOld(int width, int height, RotationSystemTable rotationSystemTable)
        {
            Width = width;
            Height = height;

            RotationSystemTable = rotationSystemTable;

            _board = new BoardPieceType[Width * Height];

            for (int i = 0; i < _board.Length; i++)
            {
                _board[i] = BoardPieceType.None;
            }
        }

        public Piece CreatePiece(PieceType type)
        {
            return new Piece(type, RotationSystemTable.RotationSystemTableMap[type]);
        }

        public void PushPiece(PieceType type)
        {
            PushPiece(CreatePiece(type));
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
                    if (_currentPiece.DrawAt(x, y))
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

        public void RotatePiece(Rotation rotation)
        {
            if (_currentPiece == null)
                return;

            _currentPiece.Rotate(rotation, (tests) =>
            {
                var res = TestAll(tests, _currentPiece.X, _currentPiece.Y, out var firstPassedTest, out var firstPassedTestIndex);
                if (res)
                    return firstPassedTest;

                return null;
            });
            
            switch (rotation)
            {
                case Rotation.Clockwise:
                    
                    break;
                case Rotation.CounterClockwise:
                    break;
                case Rotation.Deg180:
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(rotation), rotation, null);
            }
        }

        public void Reset() => _board = new BoardPieceType[Width * Height];

        public void Update(float deltaTime)
        {
            if (_currentPiece == null)
                return;

            if (_currentPiece.Y < Height - 2)
            {
                //_currentPiece.AbsoluteY += 1f * deltaTime;
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
                if (!TestPosition(y))
                    break;

                dropY = y;
            }

            return dropY;
        }

        private bool TestPosition(int y)
        {
            if (_currentPiece == null)
                return false;

            return TestPosition(_currentPiece.PieceTable, _currentPiece.X, y);
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
            }

            firstPassedTest = null;
            firstPassedTestIndex = null;
            return false;
        }

        public BoardPieceType GetPieceAt(int x, int y) => _board[GetIndexByCoordinates(x, y)];
        public void SetPieceAt(int x, int y, BoardPieceType piece) => _board[GetIndexByCoordinates(x, y)] = piece;
        public int GetIndexByCoordinates(int x, int y) => x + Width * y;
        public int GetIndexByCoordinates(Point coordinates) => GetIndexByCoordinates(coordinates.X, coordinates.Y);
        public Point GetCoordinatesByIndex(int index) => new Point(index / Width, index / Height);
    }
}