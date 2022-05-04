using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
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

        public PieceBase CurrentPiece { get; private set; }

        public Board(int width = 10, int height = 20)
        {
            Width = width;
            Height = height;
            TotalHeight = Height + ExtraHeight;

            _cellContainer = new BoardCellContainer(Width, TotalHeight);
            CurrentPiece = new PiecePixel();
            ResetPiece(CurrentPiece);
        }

        public void PushPiece(PieceType type)
        {
            var pf = new PieceFactory();
            var piece = pf.Create(type);

            ResetPiece(piece);
            
            CurrentPiece = piece;
        }

        public void ResetPiece(PieceBase piece)
        {
            piece.CurrentRotation = PieceStartPosition.Initial;

            if (piece.OffsetType == OffsetType.BetweenCells)
                piece.X = Width / 2;
            else
                piece.X = (int) Math.Round((Width - 1) / 2.0);

            if (piece.Type == PieceType.I)
                piece.Y = Height - 1; // TODO: FIXME
            else piece.Y = Height - 2;
        }

        public void MoveLeft()
        {
            var t = Debug.TimeAction(() =>
            {
                if (TestMovement(-1, 0))
                    CurrentPiece.X -= 1;
            });
            GlobalTimeManager.AddData("MoveLeft", t);
        }

        public void MoveRight()
        {
            var t = Debug.TimeAction(() =>
            {
                if (TestMovement(1, 0))
                    CurrentPiece.X += 1;
            });
            GlobalTimeManager.AddData("MoveRight", t);
        }

        public void SoftDrop()
        {
            var t = Debug.TimeAction(() =>
            {
                if (TestMovement(0, 1))
                    CurrentPiece.Y += 1;
            });
            GlobalTimeManager.AddData("SoftDrop", t);
        }

        public void HardDrop()
        {
            var t = Debug.TimeAction(() =>
            {
                var nearestY = FindNearestY();

                if (!TryApplyPiece(CurrentPiece.CurrentPos, CurrentPiece.X, nearestY))
                    throw new Exception("Something went wrong while applying the piece");
            });

            var t2 = Debug.TimeAction(() => CheckLineClears());

            ResetPiece(CurrentPiece);
            
            GlobalTimeManager.AddData("HardDrop", t);
            GlobalTimeManager.AddData("CheckLineClears", t2);
        }

        public void Rotate(Rotation rotation)
        {
            var t = Debug.TimeAction(() =>
            {
                CurrentPiece.Rotate(rotation, kickParams => new PieceBase.WallKickCheckResult
                {
                    Success = TestRotation(kickParams, out Point? test),
                    WallKickPosition = test
                });
            });
            GlobalTimeManager.AddData("Rotate", t);
        }

        public int FindNearestY()
        {
            var y = Math.Max(CurrentPiece.Y, 0);

            for (int i = y; i <= TotalHeight; i++)
            {
                if (_cellContainer.Intersects(AdjustPositions(CurrentPiece.CurrentPos, new Point(CurrentPiece.X, i))))
                    break;

                y = i;
            }

            return y;
        }
        
        public void Reset() => _cellContainer.Reset();
        public void MoveUp() => _cellContainer.MoveUp();
        public void MoveDown(int fromY = 0) => _cellContainer.MoveDown(fromY);
        public BoardCellType GetCellAt(int x, int y) => _cellContainer.GetCellAt(x, y);
        public void SetCellAt(int x, int y, BoardCellType cell) => _cellContainer.SetCellAt(x, y, cell);
        
        
        private bool TestMovement(int xOffset, int yOffset)
        {
            // Checking piece bounds first as it is much faster
            var b = CurrentPiece.Bounds;
            if (b.X + xOffset < 0 || b.X + b.Width + xOffset > Width)
                return false;
            if (b.Y + b.Height + yOffset > TotalHeight)
                return false;

            var adjusted = AdjustPositions(CurrentPiece.CurrentPos,
                new Point(CurrentPiece.X + xOffset, CurrentPiece.Y + yOffset));

            return !_cellContainer.Intersects(adjusted);
        }
        
        private int CheckLineClears()
        {
            int linesCleared = 0;
            
            // TODO: Check only affected Y's
            for (int y = 0; y < TotalHeight; y++)
            {
                var isFull = _cellContainer.IsLineFull(y);
                if (isFull)
                {
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
                    new Point(CurrentPiece!.X, CurrentPiece!.Y) + test
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
                
                // TODO: Convert properly
                SetCellAt(point.X, point.Y, (BoardCellType)(((int)CurrentPiece.Type) + 1));
            }
            
            return true;
        }
    }
}
