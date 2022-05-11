using System;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
using Debug = Nez.Debug;

namespace Quader.Engine
{
    public enum LastMoveType
    {
        None = 0,
        Movement,
        Rotation
    }

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

        public event EventHandler<PieceBase>? PieceHardDropped;
        public event EventHandler<PieceBase>? PiecePushed;
        public event EventHandler<PieceMovedEventArgs>? PieceMoved;
        public event EventHandler<PieceBase>? PieceRotated;
        public event EventHandler<int>? LinesCleared;
        public event EventHandler? BoardChanged;

        private readonly BoardCellContainer _cellContainer;

        private int _piecesOnBoard = 0;
        public int PiecesOnBoard => _piecesOnBoard;

        public PieceBase CurrentPiece { get; private set; }

        public int CurrentCombo { get; private set; }
        public int CurrentB2B { get; private set; }

        public LastMoveType LastMoveType { get; private set; } = LastMoveType.None;

        public BoardPieceHolder PieceHolder { get; }

        public Board(int width = 10, int height = 20)
        {
            Width = width;
            Height = height;
            TotalHeight = Height + ExtraHeight;

            _cellContainer = new BoardCellContainer(Width, TotalHeight);
            PieceHolder = new BoardPieceHolder();
            CurrentPiece = new PiecePixel();
            ResetPiece(CurrentPiece);
        }

        public void SetPiece(PieceType type)
        {
            var pf = new PieceFactory();
            var piece = pf.Create(type);

            SetPiece(piece);
        }

        public void SetPiece(PieceBase piece)
        {
            ResetPiece(piece);

            CurrentPiece = piece;
            
            PiecePushed?.Invoke(this, CurrentPiece);
        }

        public void MoveLeft(int delta = 1)
        {
            if (TestMovement(-delta, 0))
            {
                CurrentPiece.X -= delta;
                LastMoveType = LastMoveType.Movement;
            }

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(-delta, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
        }

        public void MoveRight(int delta = 1)
        {
            if (TestMovement(delta, 0))
            {
                CurrentPiece.X += delta;
                LastMoveType = LastMoveType.Movement;
            }

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(delta, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
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

            LastMoveType = LastMoveType.None;
        }

        public void SoftDrop(int delta = 1)
        {
            var t = Debug.TimeAction(() =>
            {
                if (delta > 1)
                {
                    var nearestY = FindNearestY();
                    var offset = Math.Min(nearestY, delta);
                    CurrentPiece.Y = offset; // TODO: FIXME
                    //CurrentPiece.Y = Math.Min(CurrentPiece.Y, nearestY);
                }
                else if (TestMovement(0, delta))
                    CurrentPiece.Y += delta;
            });
            GlobalTimeManager.AddData("SoftDrop", t);

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(0, delta), new Point(CurrentPiece.X, CurrentPiece.Y)));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns>Lines cleared after the hard drop</returns>
        /// <exception cref="Exception">Piece cannot be applied</exception>
        public BoardMove HardDrop()
        {
            int nearestY = 0;

            var t0 = Debug.TimeAction(() => nearestY = FindNearestY());

            var t = Debug.TimeAction(() =>
            {
                if (!TryApplyPiece(CurrentPiece.CurrentPos, CurrentPiece.X, nearestY))
                    return;
                //throw new Exception("Something went wrong while applying the piece");
            });

            int linesCleared = 0;

            var t2 = Debug.TimeAction(() => linesCleared = CheckLineClears());

            GlobalTimeManager.AddData("FindNearestY", t0);
            GlobalTimeManager.AddData("TryApplyPiece", t);
            GlobalTimeManager.AddData("CheckLineClears", t2);

            var moveType = BoardMoveType.None;

            if (_piecesOnBoard == 0)
                moveType |= BoardMoveType.AllClear;

            if (linesCleared == 1)
                moveType |= BoardMoveType.Single;
            else if (linesCleared == 2)
                moveType |= BoardMoveType.Double;
            else if (linesCleared == 3)
                moveType |= BoardMoveType.Triple;
            else if (linesCleared == 4)
            {
                moveType |= BoardMoveType.Quad;
                CurrentB2B++;
            }

            if (CurrentCombo > 1)
                moveType |= BoardMoveType.Combo;
            if (CurrentB2B > 0)
                moveType |= BoardMoveType.BackToBack;

            if (CurrentPiece.Type == PieceType.T && LastMoveType == LastMoveType.Rotation)
            {
                moveType |= BoardMoveType.TSpin;

                // Handle T-Spins
                // TODO: Add correct checks for T overhangs and handle T-Spin minis
                if (linesCleared == 1)
                {
                    moveType |= BoardMoveType.TSpinSingle;
                    CurrentB2B++;
                }
                else if (linesCleared == 2)
                {
                    moveType |= BoardMoveType.TSpinDouble;
                    CurrentB2B++;
                }
                else if (linesCleared == 3)
                {
                    moveType |= BoardMoveType.TSpinTriple;
                    CurrentB2B++;
                }
            }

            //BoardMoveType.TSpinDouble
            //BoardMoveType.TSpinDoubleMini
            //BoardMoveType.TSpinSingle
            //BoardMoveType.TSpinSingleMini
            //BoardMoveType.TSpinTriple

            if (linesCleared == 0)
                CurrentCombo = 0;

            if (!moveType.HasFlag(BoardMoveType.Quad) &&
                !moveType.HasFlag(BoardMoveType.TSpinDouble) &&
                !moveType.HasFlag(BoardMoveType.TSpinDoubleMini) &&
                !moveType.HasFlag(BoardMoveType.TSpinSingle) &&
                !moveType.HasFlag(BoardMoveType.TSpinSingleMini) &&
                !moveType.HasFlag(BoardMoveType.TSpinTriple)
               )
            {
                CurrentB2B = 0;
            }

            LastMoveType = LastMoveType.None;

            PieceHardDropped?.Invoke(this, CurrentPiece);
            ResetPiece(CurrentPiece);

            return new BoardMove
            {
                LinesCleared = linesCleared,
                Type = moveType,
                Timestamp = DateTime.UtcNow,
                BackToBack = CurrentB2B,
                Combo = CurrentCombo++
            };
        }

        public void Rotate(Rotation rotation)
        {
            var t = Debug.TimeAction(() =>
            {
                CurrentPiece.Rotate(rotation, kickParams =>
                {
                    var success = TestRotation(kickParams, out Point? test);

                    if (success)
                        LastMoveType = LastMoveType.Rotation;

                    return new PieceBase.WallKickCheckResult
                    {
                        Success = success,
                        WallKickPosition = test
                    };
                });
            });
            GlobalTimeManager.AddData("Rotate", t);

            PieceRotated?.Invoke(this, CurrentPiece);
        }

        public int FindNearestY()
        {
            var y = Math.Max(CurrentPiece.Y, 0);

            for (int i = y; i <= TotalHeight; i++)
            {
                if (_cellContainer.Intersects(BoardUtils.AdjustPositions(CurrentPiece.CurrentPos, new Point(CurrentPiece.X, i))))
                    break;

                y = i;
            }

            return y;
        }

        private int _lastGarbageLineX = -1;
        public void PushGarbage(int garbageLines, int messiness = 0)
        {
            int garbageHoleX;

            if (_lastGarbageLineX == -1)
                garbageHoleX = Random.Shared.Next(0, Width);
            else
                garbageHoleX = _lastGarbageLineX; // TODO: Fix messiness

            for (int i = 0; i < garbageLines; i++)
            {
                MoveUp();
                _cellContainer.SetLine(TotalHeight - 1, CreateGarbageRow(garbageHoleX));
            }

            BoardChanged?.Invoke(this, EventArgs.Empty);
        }

        private BoardCellType[] CreateGarbageRow(int holeX)
        {
            BoardCellType[] row = new BoardCellType[Width];

            for (int i = 0; i < Width; i++)
            {
                if (i == holeX)
                    row[i] = BoardCellType.None;
                else
                    row[i] = BoardCellType.Garbage;
            }

            return row;
        }

        public void ForceUpdate()
        {
            CheckLineClears();

            BoardChanged?.Invoke(this, EventArgs.Empty);
        }
        
        public void Reset()
        {
            LastMoveType = LastMoveType.None;
            CurrentB2B = 0;
            CurrentCombo = 0;
            _piecesOnBoard = 0;

            _cellContainer.Reset();

            BoardChanged?.Invoke(this, EventArgs.Empty);
        }
        public void MoveUp()
        {
            _cellContainer.MoveUp();
            BoardChanged?.Invoke(this, EventArgs.Empty);
        }
        public void MoveDown(int fromY = 0) => _cellContainer.MoveDown(fromY);
        public BoardCellType GetCellAt(int x, int y) => _cellContainer.GetCellAt(x, y);

        public void SetCellAt(int x, int y, BoardCellType cell, bool needsUpdate = false)
        {
            _cellContainer.SetCellAt(x, y, cell);
            if (needsUpdate)
                BoardChanged?.Invoke(this, EventArgs.Empty);
        }

        public void SetCellAtRange(KeyValuePair<Point, BoardCellType>[] cells)
        {
            foreach (var kv in cells)
            {
                _cellContainer.SetCellAt(kv.Key.X, kv.Key.Y, kv.Value);
            }

            BoardChanged?.Invoke(this, EventArgs.Empty);
        }

        public bool IsOutOfBounds(Point p) => _cellContainer.IsOutOfBounds(p);

        public BoardEncoding Encode(bool includeQueue = true)
        {
            var res = "";

            for (int y = 0; y < TotalHeight; y++)
            {
                for (int x = 0; x < Width; x++)
                {
                    res += GetCellAt(x, y) == BoardCellType.None ? " " : "X";
                }
            }

            return new BoardEncoding
            {
                Code = res,
                Height = Height,
                Width = Width,
                TotalHeight = TotalHeight
            };
        }

        public void Decode(BoardEncoding encoding, bool includeQueue = true)
        {
            Reset();

            if (encoding.Width != Width)
                throw new Exception("Invalid Width");
            if (encoding.Height != Height)
                throw new Exception("Invalid Height");

            int x = 0;
            int y = 0;
            int piecesOnBoard = 0;

            foreach (var ch in encoding.Code)
            {
                if (ch == 'X')
                {
                    SetCellAt(x, y, BoardCellType.Garbage);
                    piecesOnBoard++;
                }

                x++;
                if (x >= Width)
                {
                    y++;
                    x = 0;
                }
            }

            _piecesOnBoard = piecesOnBoard;

            BoardChanged?.Invoke(this, EventArgs.Empty);
        }
        
        private bool TestMovement(int xOffset, int yOffset)
        {
            // Checking piece bounds first as it is much faster
            var b = CurrentPiece.Bounds;
            if (b.X + xOffset < 0 || b.X + b.Width + xOffset > Width)
                return false;
            if (b.Y + b.Height + yOffset > TotalHeight)
                return false;

            var adjusted = BoardUtils.AdjustPositions(CurrentPiece.CurrentPos,
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
                    _piecesOnBoard -= Width;
                    
                    MoveDown(y);
                }
            }

            if (linesCleared > 0)
            {
                LinesCleared?.Invoke(this, linesCleared);
                BoardChanged?.Invoke(this, EventArgs.Empty);
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
                
                var adjusted = BoardUtils.AdjustPositions(
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

        private bool TryApplyPiece(Point[] points, int x, int y)
        {
            var adjusted = TimeAction(() => BoardUtils.AdjustPositions(points, new Point(x, y)), out var elapsed);
            GlobalTimeManager.AddData("TryApplyPiece.AdjustPositions", elapsed);

            bool res = true;

            var a = Debug.TimeAction(() =>
            {
                foreach (var point in adjusted)
                {
                    var piece = GetCellAt(point.X, point.Y);

                    if (piece != BoardCellType.None)
                        res = false;
                    SetCellAt(point.X, point.Y, CurrentPiece.BoardCellType);
                    _piecesOnBoard++;
                }
            });

            GlobalTimeManager.AddData("TryApplyPiece.GetSetCell", a);

            var bc = Debug.TimeAction(() => BoardChanged?.Invoke(this, EventArgs.Empty));
            GlobalTimeManager.AddData("TryApplyPiece.BoardChanged.Invoke", bc);
            /*SetCellAtRange(
                adjusted.Select(
                        point => new KeyValuePair<Point, BoardCellType>(point, CurrentPiece.BoardCellType)
                    )
                    .ToArray()
            );*/

            return res;
        }

        private T TimeAction<T>(Func<T> func, out TimeSpan elapsed)
        {
            var stopwatch = new Stopwatch();
            stopwatch.Start();
            
            var res = func();
            stopwatch.Stop();

            elapsed = stopwatch.Elapsed;

            return res;
        }
    }
}
