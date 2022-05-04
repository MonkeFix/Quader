using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;
using Quader.Engine.RotationEncoder;

namespace Quader.Engine
{
    public class Board
    {
        /// <summary>
        /// Extra height of the board. Used for cases when player receives garbage with ability to spawn a new piece.
        /// </summary>
        public static readonly int ExtraHeight = 20;
        
        public int Width { get; }
        public int Height { get; }
        public int TotalHeight { get; }
        
        public Queue<Point[]> TestQueue { get; } = new ();
        

        private readonly BoardCellContainer _cellContainer;

        private PieceBase? _currentPiece = null;

        public PieceBase? CurrentPiece => _currentPiece;

        public Board(int width = 10, int height = 20)
        {
            Width = width;
            Height = height;
            TotalHeight = Height + ExtraHeight;

            _cellContainer = new BoardCellContainer(Width, TotalHeight);
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

            if (piece.Type == PieceType.I)
                piece.Y = 19; // TODO: FIXME
            else piece.Y = 18;
        }

        public void MoveLeft()
        {
            if (_currentPiece == null)
                return;

            if (TestMovement(-1, 0))
                _currentPiece!.X -= 1;
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
                
            CheckLineClears();
            
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

            for (int i = y; i <= TotalHeight; i++)
            {
                if (_cellContainer.Intersects(AdjustPositions(_currentPiece.CurrentPos, new Point(_currentPiece.X, i))))
                    break;

                y = i;
            }

            return y;
        }

        private bool TestMovement(int xOffset, int yOffset)
        {
            if (_currentPiece == null)
                return false;

            // Checking bounds first as it is much faster
            var b = _currentPiece.Bounds;
            if (b.X + xOffset < 0 || b.X + b.Width + xOffset > Width)
                return false;
            if (b.Y + b.Height + yOffset > TotalHeight)
                return false;

            var adjusted = AdjustPositions(_currentPiece.CurrentPos,
                new Point(_currentPiece.X + xOffset, _currentPiece.Y + yOffset));

            return !_cellContainer.Intersects(adjusted);
        }
        
        public void Reset() => _cellContainer.Reset();
        public void MoveUp() => _cellContainer.MoveUp();
        public void MoveDown(int fromY = 0) => _cellContainer.MoveDown(fromY);
        public BoardCellType GetCellAt(int x, int y) => _cellContainer.GetCellAt(x, y);
        public void SetCellAt(int x, int y, BoardCellType cell) => _cellContainer.SetCellAt(x, y, cell);
        
        
        private int CheckLineClears()
        {
            int linesCleared = 0;
            
            for (int y = 0; y < TotalHeight; y++)
            {
                var isFull = _cellContainer.IsLineFull(y);
                if (isFull)
                {
                    _cellContainer.ClearLine(y);
                    linesCleared++;
                    
                    MoveDown(y);
                }
            }

            return linesCleared;
        }

        private bool TestRotation(PieceBase.WallKickCheckParams kickParams, out Point? firstSuccessfulTest)
        {
            var tests = kickParams.Tests;
            var expectedPos = kickParams.ExpectedPos;

            TestQueue.Clear();
            
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

                var intersects = _cellContainer.Intersects(adjusted);

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

        private bool TryApplyPiece(Point[] points, int x, int y)
        {
            var adjusted = AdjustPositions(points, new Point(x, y));

            foreach (var point in adjusted)
            {
                var piece = GetCellAt(point.X, point.Y);
                if (piece != BoardCellType.None)
                    return false;
                
                SetCellAt(point.X, point.Y, (BoardCellType)(((int)_currentPiece.Type) + 1));
            }
            
            return true;
        }
    }
}
