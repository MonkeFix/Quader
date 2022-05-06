using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
using Quader.Engine.RotationEncoder;
using Quader.InputMgr;

namespace Quader.Engine
{
    public class Board : IInputHandleable
    {
        /// <summary>
        /// Extra height of the board. Used for cases when player receives garbage with ability to spawn a new piece.
        /// </summary>
        public static readonly int ExtraHeight = 20;
        
        public int Width { get; }
        public int Height { get; }
        public int TotalHeight { get; }
        
        public Queue<Point[]> TestQueue { get; } = new ();

        public event EventHandler<PieceBase>? PieceHardDropped;
        public event EventHandler<PieceBase>? PiecePushed;
        public event EventHandler<PieceMovedEventArgs>? PieceMoved;
        public event EventHandler<PieceBase>? PieceRotated;
        public event EventHandler<int>? LinesCleared; 


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

            PushPiece(piece);
        }

        public void PushPiece(PieceBase piece)
        {
            ResetPiece(piece);

            CurrentPiece = piece;

            PiecePushed?.Invoke(this, CurrentPiece);
        }

        public void ResetPiece(PieceBase piece)
        {
            piece.CurrentRotation = PieceStartPosition.Initial;

            if (piece.OffsetType == OffsetType.BetweenCells)
                piece.X = Width / 2;
            else
                piece.X = (int) Math.Round((Width - 1) / 2.0);

            if (piece.Type == PieceType.I)
                piece.Y = Height - 1; 
            else piece.Y = Height - 2;
        }

        public void MoveLeft(int delta = 1)
        {
            var t = Debug.TimeAction(() =>
            {
                if (TestMovement(-delta, 0))
                    CurrentPiece.X -= delta;
            });
            GlobalTimeManager.AddData("MoveLeft", t);

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(-delta, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
        }

        public void MoveRight(int delta = 1)
        {
            var t = Debug.TimeAction(() =>
            {
                if (TestMovement(delta, 0))
                    CurrentPiece.X += delta;
            });
            GlobalTimeManager.AddData("MoveRight", t);

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(delta, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
        }

        public void Move(int direction)
        {
            if (TestMovement(direction, 0))
                CurrentPiece.X += direction;

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(direction, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
        }

        public void SoftDrop(int delta = 1)
        {
            var t = Debug.TimeAction(() =>
            {
                if (TestMovement(0, delta))
                    CurrentPiece.Y += delta;
            });
            GlobalTimeManager.AddData("SoftDrop", t);

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(0, delta), new Point(CurrentPiece.X, CurrentPiece.Y)));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns>Lines cleared after the hard drop</returns>
        /// <exception cref="Exception"></exception>
        public int HardDrop()
        {
            var t = Debug.TimeAction(() =>
            {
                var nearestY = FindNearestY();

                if (!TryApplyPiece(CurrentPiece.CurrentPos, CurrentPiece.X, nearestY))
                    throw new Exception("Something went wrong while applying the piece");
            });

            int linesCleared = 0;

            var t2 = Debug.TimeAction(() => linesCleared = CheckLineClears());

            PieceHardDropped?.Invoke(this, CurrentPiece);

            ResetPiece(CurrentPiece);

            GlobalTimeManager.AddData("HardDrop", t);
            GlobalTimeManager.AddData("CheckLineClears", t2);

            return linesCleared;
        }

        public void HoldPiece()
        {
            
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

            PieceRotated?.Invoke(this, CurrentPiece);
        }

        public int FindNearestY()
        {
            //return 20;

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
        public bool IsOutOfBounds(Point p) => _cellContainer.IsOutOfBounds(p);
        
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

            if (linesCleared > 0)
                LinesCleared?.Invoke(this, linesCleared);

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
