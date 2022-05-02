using System;
using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;

namespace Quader.Engine
{
    public class Board
    {
        public int Width { get; }
        public int Height { get; }
        

        private BoardPieceType[] _boardLayout;

        private PieceBase? _currentPiece = null;

        public PieceBase? CurrentPiece => _currentPiece;
        
        public Board(int width = 10, int height = 20)
        {
            Width = width;
            Height = height;

            _boardLayout = new BoardPieceType[width * height];
            
            Reset();
        }

        public void PushPiece(PieceType type)
        {
            var pf = new PieceFactory();
            var piece = pf.Create(type);
            piece.X = 5;
            _currentPiece = piece;
            _currentPiece.X = 5;
        }

        public void Reset()
        {
            for (int i = 0; i < _boardLayout.Length; i++)
            {
                _boardLayout[i] = BoardPieceType.None;
            }
        }

        public void MoveLeft()
        {
            if (_currentPiece == null)
                return;

            if (TestMovement(-1, 0))
                _currentPiece.X -= 1;
        }

        public void MoveRight()
        {
            if (_currentPiece == null)
                return;

            if (TestMovement(1, 0))
                _currentPiece.X += 1;
        }

        public void SoftDrop()
        {
            if (_currentPiece == null)
                return;

            if (TestMovement(0, 1))
                _currentPiece.Y += 1;
        }

        public void Rotate(Rotation rotation)
        {
            if (_currentPiece == null)
                return;

            //_currentPiece.RotateSimple(rotation);
            _currentPiece.Rotate(rotation, kickParams => new PieceBase.WallKickCheckResult
            {
                Success = TestRotation(kickParams, out Point? test),
                WallKickPosition = test
            });
        }

        private bool TestMovement(int xOffset, int yOffset)
        {
            var b = _currentPiece!.Bounds;

            if (b.X + xOffset < 0)
                return false;

            if (b.X + xOffset + b.Width > Width)
                return false;

            if (b.Y + yOffset + b.Height > Height)
                return false;

            return true;
        }

        private bool TestRotation(PieceBase.WallKickCheckParams kickParams, out Point? firstSuccessfulTest)
        {
            var tests = kickParams.Tests;
            var expectedPos = kickParams.ExpectedPos;
            
            firstSuccessfulTest = null;

            foreach (var test in tests)
            {
                // var bounds = PieceBase.GetBounds(expectedPos, _currentPiece!.X + test.X, _currentPiece!.Y + test.Y);
                var adjusted = AdjustPositions(
                    expectedPos,
                    new Point(_currentPiece!.X, _currentPiece!.Y) + test
                );

                var intersects = Intersects(adjusted, out bool isFloorKick);

                if (!intersects)
                {
                    firstSuccessfulTest = test;

                    return true;
                }

                if (isFloorKick)
                {
                    firstSuccessfulTest = new Point(0, -1);

                    return true;
                }
            }
            
            return false;
        }

        private Point[] AdjustPositions(Point[] data, Point offset)
        {
            var newData = new Point[data.Length];
            
            for (int i = 0; i < data.Length; i++)
            {
                newData[i] = new Point(data[i].X + offset.X, data[i].Y + offset.Y);
                // data[i].X += offset.X;
                // data[i].Y += offset.Y;
            }

            return newData;
        }

        private bool Intersects(Point[] points, out bool isFloorKick)
        {
            isFloorKick = false;
            
            foreach (var point in points)
            {
                if (point.Y < 0)
                    continue; // Skip tests upper than the board's border
                
                if (point.X < 0 || point.X >= Width)
                    return true;

                if (point.Y >= Height)
                {
                    isFloorKick = true;
                    return true;
                }

                if (GetPieceAt(point.X, point.Y) != BoardPieceType.None)
                    return true;
            }

            return false;
        }
        
        public BoardPieceType GetPieceAt(int x, int y) => _boardLayout[GetIndexByCoordinates(x, y)];
        public void SetPieceAt(int x, int y, BoardPieceType piece) => _boardLayout[GetIndexByCoordinates(x, y)] = piece;
        public int GetIndexByCoordinates(int x, int y) => x + Width * y;
    }
}
