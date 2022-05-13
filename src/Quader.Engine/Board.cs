using System;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
using Quader.Engine.Replays;
using Quader.Engine.Serialization;
using Quader.Engine.Settings;
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

        public event EventHandler<BoardMove>? PieceHardDropped;
        public event EventHandler<PieceBase>? PiecePushed;
        public event EventHandler<PieceMovedEventArgs>? PieceMoved;
        public event EventHandler<PieceBase>? PieceRotated;
        public event EventHandler<int>? LinesCleared;
        public event EventHandler? BoardChanged;

        private readonly BoardCellContainer _cellContainer;

        private int _piecesOnBoard;
        public int PiecesOnBoard => _piecesOnBoard;

        public PieceBase CurrentPiece { get; private set; }

        public int CurrentCombo { get; private set; }
        public int CurrentB2B { get; private set; }

        public float CurrentGravity { get; private set; }
        public float CurrentLock { get; private set; }

        public LastMoveType LastMoveType { get; private set; } = LastMoveType.None;

        public BoardPieceHolder PieceHolder { get; }

        // Used for smooth gravity handling
        private float _intermediateY;

        public GravitySettings GravitySettings { get; }

        /*public Board(int width = 10, int height = 20)
        {
            Width = width;
            Height = height;
            TotalHeight = Height + ExtraHeight;

            _cellContainer = new BoardCellContainer(Width, TotalHeight);
            PieceHolder = new BoardPieceHolder();
            CurrentPiece = new PiecePixel();
            ResetPiece(CurrentPiece);
        }*/

        public Board(GameSettings settings)
        {
            if (settings == null)
                throw new ArgumentNullException(nameof(settings),
                    "Settings cannot be null. Use GameSettings.Default");

            GravitySettings = settings.Gravity;

            Width = settings.Board.BoardWidth;
            Height = settings.Board.BoardHeight;
            TotalHeight = settings.Board.BoardHeight * 2;

            _cellContainer = new BoardCellContainer(Width, TotalHeight);
            PieceHolder = new BoardPieceHolder();
            CurrentPiece = new PiecePixel();
            ResetPiece(CurrentPiece);

            CurrentGravity = settings.Gravity.BaseGravity;
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

        /// <summary>
        /// Moves the current piece left by specified delta
        /// </summary>
        /// <param name="delta">Move left delta offset. Must be positive</param>
        public void PieceMoveLeft(int delta = 1)
        {
            if (TestMovement(-delta, 0))
            {
                CurrentPiece.X -= delta;
                LastMoveType = LastMoveType.Movement;
            }

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(-delta, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
        }

        /// <summary>
        /// Moves the current piece right by specified delta
        /// </summary>
        /// <param name="delta">Move right delta offset. Must be positive</param>
        public void PieceMoveRight(int delta = 1)
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
            _intermediateY = 0;
            piece.CurrentRotation = PieceStartPosition.Initial;

            if (piece.OffsetType == OffsetType.BetweenCells)
                piece.X = Width / 2;
            else
                piece.X = (int) Math.Round((Width - 1) / 2.0);

            if (piece.Type == PieceType.I)
                piece.Y = Height - 1; 
            else piece.Y = Height - 2;

            LastMoveType = LastMoveType.None;

            _intermediateY = 0;
            CurrentLock = GravitySettings.LockDelay;
        }

        public bool SoftDrop(int delta = 1)
        {
            var res = true;

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
                else
                    res = false;
            });
            GlobalTimeManager.AddData("SoftDrop", t);

            PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(0, delta), new Point(CurrentPiece.X, CurrentPiece.Y)));

            return res;
        }

        public BoardMove HardDrop()
        {
            int nearestY = 0;

            var t0 = Debug.TimeAction(() => nearestY = FindNearestY());
            
            if (!TryApplyPiece(CurrentPiece.CurrentPos, CurrentPiece.X, nearestY))
                return new BoardMove();

            var moveType = BoardMoveModificators.None;

            TSpinType tSpinType = TSpinType.None;

            // Handle T-Spins - the last move must be a rotation.
            // If there's an overhang - it's a regular T-Spin, if not, it's a T-Spin Mini
            if (CurrentPiece.Type == PieceType.T && LastMoveType == LastMoveType.Rotation)
            {
                tSpinType = CheckTOverhang();

                if (tSpinType == TSpinType.Full)
                    moveType |= BoardMoveModificators.TSpin;
                else if (tSpinType == TSpinType.Mini)
                    moveType |= BoardMoveModificators.TSpinMini;
            }

            var linesCleared = CheckLineClears();

            GlobalTimeManager.AddData("FindNearestY", t0);

            CreateBoardMoveType(ref moveType, linesCleared, tSpinType);

            LastMoveType = LastMoveType.None;

            var bm = new BoardMove
            {
                LinesCleared = linesCleared,
                Modificators = moveType,
                Timestamp = DateTime.UtcNow,
                BackToBack = CurrentB2B,
                Combo = CurrentCombo++,
                Success = true
            };

            PieceHardDropped?.Invoke(this, bm);
            ResetPiece(CurrentPiece);

            return bm;
        }

        /// <summary>
        /// Helper method that updates gravity and position of the current piece.
        /// Meant to be called every update cycle tick (once in 1/FPS seconds)
        /// </summary>
        /// <param name="dt">Delta time, time difference between current and previous frames</param>
        public void UpdateGravity(float dt)
        {
            _intermediateY += CurrentGravity * dt;

            bool softDropTest = true;
            if (_intermediateY > 1.0f)
            {
                var diff = Math.Max((int)(_intermediateY - 1.0f), 1);
                for (int i = 0; i < diff; i++)
                {
                    softDropTest = SoftDrop();
                }
                
                _intermediateY = 0;
            }

            if (!softDropTest)
            {
                CurrentLock -= 1 * (dt * 1000);
            }

            if (CurrentLock <= 0)
                HardDrop();

            CurrentGravity += GravitySettings.GravityIncrease * dt;
        }

        private void CreateBoardMoveType(ref BoardMoveModificators moveType, int linesCleared, TSpinType tSpinType)
        {
            // All Clear - No pieces on the board after a move
            if (_piecesOnBoard == 0)
                moveType |= BoardMoveModificators.AllClear;

            // Basic clears - 4 lines is the max
            if (linesCleared == 1)
                moveType |= BoardMoveModificators.Single;
            else if (linesCleared == 2)
                moveType |= BoardMoveModificators.Double;
            else if (linesCleared == 3)
                moveType |= BoardMoveModificators.Triple;
            else if (linesCleared == 4)
            {
                moveType |= BoardMoveModificators.Quad;
                CurrentB2B++;
            }

            // Combo handling - the higher the combo - the bigger the spike
            if (CurrentCombo > 1)
                moveType |= BoardMoveModificators.Combo1;
            if (CurrentCombo >= 6)
                moveType |= BoardMoveModificators.Combo2;
            if (CurrentCombo >= 10)
                moveType |= BoardMoveModificators.Combo3;
            if (CurrentCombo >= 15)
                moveType |= BoardMoveModificators.Combo4;
            if (CurrentCombo >= 18)
                moveType |= BoardMoveModificators.Combo5;

            // If move does not contain a Quad, T-Spin or T-Spin Mini, B2B status is 0
            if (
                linesCleared > 0 &&
                !moveType.HasFlag(BoardMoveModificators.Quad) &&
                !moveType.HasFlag(BoardMoveModificators.TSpin) &&
                !moveType.HasFlag(BoardMoveModificators.TSpinMini)
            )
            {
                CurrentB2B = 0;
            }

            // Multiple successive T-Spins (including minis) or Quads
            if (CurrentB2B > 0)
                moveType |= BoardMoveModificators.BackToBack1;
            if (CurrentB2B >= 5)
                moveType |= BoardMoveModificators.BackToBack2;
            if (CurrentB2B >= 10)
                moveType |= BoardMoveModificators.BackToBack3;
            if (CurrentB2B >= 30)
                moveType |= BoardMoveModificators.BackToBack4;
            if (CurrentB2B >= 60)
                moveType |= BoardMoveModificators.BackToBack5;

            if (tSpinType != TSpinType.None)
                CurrentB2B++;

            // Break the combo if cleared 0 lines
            if (linesCleared == 0)
                CurrentCombo = 0;
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

        // ReSharper disable once InconsistentNaming
        enum TSpinType
        {
            None, Full, Mini
        }

        public Point[] PointsChecked = new Point[4];


        private TSpinType CheckTOverhang()
        {
            // Safety check
            if (CurrentPiece.Type != PieceType.T)
                return TSpinType.None;

            // A block must be in any of the blocks in this graph:
            // #T#
            // TTT
            // #T#
            // T - T Piece
            // # - Block

            var pieceX = CurrentPiece.X;
            var pieceY = CurrentPiece.Y;

            var topLeft = new Point(pieceX - 1, pieceY - 1);
            var topRight = new Point(pieceX + 1, pieceY - 1);
            var bottomLeft = new Point(pieceX - 1, pieceY + 1);
            var bottomRight = new Point(pieceX + 1, pieceY + 1);

            var pArr = new[] { topLeft, topRight, bottomLeft, bottomRight };

            PointsChecked = pArr;
                
            var oobOverhangs = 0; // out of bounds
            var nonOobOverhangs = 0;

            foreach (var p in pArr)
            {
                if (IsOutOfBounds(p))
                    oobOverhangs++;
                else
                {
                    var cell = GetCellAt(p.X, p.Y);
                    if (cell != BoardCellType.None)
                        nonOobOverhangs++;
                }
            }

            if (oobOverhangs + nonOobOverhangs > 4) // We cannot have more than 4 blocks checked in any case!
                throw new Exception("Something went wrong!");

            if (oobOverhangs > 0 && nonOobOverhangs > 0)
                return TSpinType.Mini;

            if (oobOverhangs == 0 && nonOobOverhangs >= 3)
                return TSpinType.Full;

            return TSpinType.None;
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
            _intermediateY = 0;
            _lastGarbageLineX = -1;

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

        public BoardEncoding Encode()
        {
            return this.Serialize();
        }

        public void Decode(BoardEncoding encoding)
        {
            this.Deserialize(encoding, out _piecesOnBoard);

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

            var b = CurrentPiece.Bounds;

            for (int y = Math.Max(b.Top, 0); y < TotalHeight; y++)
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
                // We need to revert the Y axis in order to perform correct checks
                var test = new Point(t.X, -t.Y);

                var adjusted = BoardUtils.AdjustPositions(
                    expectedPos,
                    new Point(CurrentPiece.X, CurrentPiece.Y) + test
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
