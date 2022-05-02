using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;
using Quader.Engine.RotationEncoder;

namespace Quader.Engine
{
    public class Board
    {
        public int Width { get; }
        public int Height { get; }
        

        // TODO: Make board layout to be from down to up (0,0 at the bottom left corner instead of top left corner)
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

            ResetPiece(piece);
            
            _currentPiece = piece;
        }

        public void ResetPiece(PieceBase piece)
        {
            piece.CurrentRotation = PieceStartPosition.Initial;

            if (piece.OffsetType == OffsetType.BetweenCells)
                piece.X = Width / 2;
            else
                piece.X = (int) Math.Round((Width - 1) / 2.0);

            piece.Y = 0;
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

        public void HardDrop()
        {
            if (_currentPiece == null)
                return;

            var nearestY = FindNearestY();

            if (!TryApplyPiece(_currentPiece.CurrentPos, _currentPiece.X, nearestY))
                throw new Exception("Something went wrong while applying the piece");
                
            ResetPiece(_currentPiece);
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

        public int FindNearestY()
        {
            if (_currentPiece == null)
                return 0;

            var y = Math.Max(_currentPiece.Y, 0);

            for (int i = y; i <= Height; i++)
            {
                if (Intersects(AdjustPositions(_currentPiece.CurrentPos, new Point(_currentPiece.X, i))))
                    break;

                y = i;
            }

            return y;
        }

        private bool TestMovement(int xOffset, int yOffset)
        {
            if (_currentPiece == null)
                return false;

            var adjusted = AdjustPositions(_currentPiece.CurrentPos,
                new Point(_currentPiece.X + xOffset, _currentPiece.Y + yOffset));

            return !Intersects(adjusted);
        }

        public Queue<Point[]> TestQueue { get; private set; } = new Queue<Point[]>();

        private bool TestRotation(PieceBase.WallKickCheckParams kickParams, out Point? firstSuccessfulTest)
        {
            var tests = kickParams.Tests;
            var expectedPos = kickParams.ExpectedPos;

            TestQueue = new Queue<Point[]>();
            
            firstSuccessfulTest = null;

            foreach (var t in tests)
            {
                var test = new Point(t.X, -t.Y); // TODO: We need to revert the Y axis to perform correct checks
                
                // TODO: For 180deg rotation we need to perform two consecutive tests:
                //      Right -> Perform Tests -> Right again -> Perform Tests -> Done
                
                var adjusted = AdjustPositions(
                    expectedPos,
                    new Point(_currentPiece!.X, _currentPiece!.Y) + test
                );
                
                TestQueue.Enqueue(adjusted);

                var intersects = Intersects(adjusted);

                if (!intersects)
                {
                    firstSuccessfulTest = test;

                    return true;
                }
            }
            
            return false;
        }

        private Point[] AdjustPositions(Point[] data, Point offset)
        {
            // TODO: Get rid of this method and perform all calculations in the PieceBase class on demand
            //  as this method takes a lot of memory
            var newData = new Point[data.Length];
            
            for (int i = 0; i < data.Length; i++)
            {
                 newData[i] = new Point(data[i].X + offset.X, data[i].Y + offset.Y);
            }

            return newData;
        }

        private bool Intersects(Point[] points)
        {
            foreach (var point in points)
            {
                if (point.Y < 0)
                    continue; // Skip tests upper than the board's border
                
                if (point.X < 0 || point.X >= Width)
                    return true;

                if (point.Y >= Height)
                {
                    return true;
                }

                if (GetPieceAt(point.X, point.Y) != BoardPieceType.None)
                    return true;
            }

            return false;
        }

        private bool TryApplyPiece(Point[] points, int x, int y)
        {
            var adjusted = AdjustPositions(points, new Point(x, y));

            foreach (var point in adjusted)
            {
                var piece = GetPieceAt(point.X, point.Y);
                if (piece != BoardPieceType.None)
                    return false;
                
                SetPieceAt(point.X, point.Y, (BoardPieceType)(int)_currentPiece.Type);
            }
            
            return true;
        }


        public bool IsOutOfBounds(Point p) => IsOutOfBoundsExceptTop(p) || p.Y < 0;
        public bool IsOutOfBoundsExceptTop(Point p) => p.X < 0 || p.X >= Width || p.Y >= Height;
        
        public BoardPieceType GetPieceAt(int x, int y) => _boardLayout[GetIndexByCoordinates(x, y)];
        public void SetPieceAt(int x, int y, BoardPieceType piece) => _boardLayout[GetIndexByCoordinates(x, y)] = piece;
        public int GetIndexByCoordinates(int x, int y) => x + Width * y;
    }
}
