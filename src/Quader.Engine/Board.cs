﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
using Quader.Engine.Replays;
using Quader.Engine.Serialization;
using Quader.Engine.Settings;
using Random = System.Random;

namespace Quader.Engine
{
    public enum LastMoveType
    {
        None,
        Rotation,
        Movement
    }

    public partial class Board
    {
        /// <summary>
        /// Extra height of the board. Used for cases when player receives garbage with ability to spawn a new piece.
        /// </summary>
        public readonly int ExtraHeight;
        
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
        public event EventHandler<int>? GarbageReceived;
        public event EventHandler<int>? AttackReceived;
        public event EventHandler? Reseted;
        public event EventHandler<GameState>? StateChanged; 

        /// <summary>
        /// Fires when board cannot spawn a new piece. It usually means that the player just lost.
        /// </summary>
        public event EventHandler? PieceCannotBeSpawned; 

        private readonly BoardCellContainer _cellContainer;

        private int _piecesOnBoard;
        public int PiecesOnBoard => _piecesOnBoard;

        public PieceBase CurrentPiece { get; private set; }

        public int CurrentCombo { get; private set; } = 1;
        public int CurrentB2B { get; private set; }

        public float CurrentGravity { get; private set; }
        public float CurrentLock { get; private set; }

        public float GarbageDelayMs => AttackSettings.GarbageDelayMs;
        public float GarbageDelayCooldown { get; private set; }

        public LastMoveType LastMoveType { get; private set; } = LastMoveType.None;
        

        // Used for smooth gravity handling
        private float _intermediateY;

        public float IntermediateY => _intermediateY;

        public BoardMove LastMove { get; private set; }

        public GravitySettings GravitySettings { get; }
        public AttackSettings AttackSettings { get; }

        public Board(GameSettings settings)
        {
            if (settings == null)
                throw new ArgumentNullException(nameof(settings),
                    "Settings cannot be null. Use GameSettings.Default");

            GravitySettings = settings.Gravity;
            AttackSettings = settings.Attack;

            Width = settings.Board.BoardWidth;
            Height = settings.Board.BoardHeight;
            ExtraHeight = settings.Board.BoardHeight;
            TotalHeight = settings.Board.BoardHeight * 2;

            _cellContainer = new BoardCellContainer(Width, TotalHeight);
            CurrentPiece = new PiecePixel();
            ResetPiece(CurrentPiece);

            GarbageDelayCooldown = AttackSettings.GarbageDelayMs;
            
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

            if (!TestMovement(0, 0))
                PieceCannotBeSpawned?.Invoke(this, EventArgs.Empty);

            PiecePushed?.Invoke(this, CurrentPiece);
        }

        private bool _yNeedsUpdate = true;
        private int _yToCheck;

        /// <summary>
        /// Helper method that updates gravity and position of the current piece.
        /// Meant to be called every update cycle tick (once in 1/FPS seconds)
        /// </summary>
        /// <param name="dt">Delta time, time difference between current and previous frames</param>
        public void UpdateGravity(float dt)
        {
            _intermediateY += CurrentGravity * dt;

            if (_yNeedsUpdate)
            {
                _yToCheck = FindNearestY();
                _yNeedsUpdate = false;
            }

            if (_intermediateY > 1.0f)
            {
                var diff = Math.Max((int)(_intermediateY - 1.0f), 1);
                for (int i = 0; i < diff; i++)
                {
                    SoftDrop();
                    _yNeedsUpdate = true;
                }

                _intermediateY = 0;
            }

            if (_yToCheck == CurrentPiece.Y)
            {
                CurrentLock -= 1 * dt; 
            }

            if (CurrentLock <= 0)
                HardDrop();

            CurrentGravity += GravitySettings.GravityIncrease * dt;

            if (IncomingDamage.Any())
            {
                GarbageDelayCooldown -= dt * 1000;
            }
            else
            {
                GarbageDelayCooldown = GarbageDelayMs;
            }

            if (GarbageDelayCooldown <= 0)
            {
                GarbageDelayCooldown = 0;
            }
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

        

        public void Attack(BoardMove move)
        {
            Attack(CalculateAttack(move));
        }

        public void Attack(int attackLines)
        {
            if (attackLines > 0)
            {
                _attackQueue.AddBack(attackLines);
                AttackReceived?.Invoke(this, attackLines);
            }
        }

        private readonly Deque<int> _attackQueue = new ();

        public IEnumerable<int> IncomingDamage => _attackQueue.ToArray();

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

            _piecesOnBoard += Width - 1;

            BoardChanged?.Invoke(this, EventArgs.Empty);
            GarbageReceived?.Invoke(this, garbageLines);
        }

        public void PushSolidLine(int lines)
        {
            for (int i = 0; i < lines; i++)
            {
                MoveUp();
                _cellContainer.SetLine(TotalHeight - 1, CreateGarbageRow(-1));
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

        public void Reset()
        {
            LastMoveType = LastMoveType.None;
            CurrentB2B = 0;
            CurrentCombo = 1;
            _piecesOnBoard = 0;
            _intermediateY = 0;
            _lastGarbageLineX = -1;

            CurrentGravity = GravitySettings.BaseGravity;

            _cellContainer.Reset();
            _attackQueue.Clear();

            Reseted?.Invoke(this, EventArgs.Empty);
            BoardChanged?.Invoke(this, EventArgs.Empty);
        }
        public void MoveUp()
        {
            _cellContainer.MoveUp();
            BoardChanged?.Invoke(this, EventArgs.Empty);
        }
        public void MoveDown(int fromY = 0) => _cellContainer.MoveDown(fromY);
        public BoardCellType GetCellAt(int x, int y) => _cellContainer.GetCellAt(x, y);
        public bool[] ToBoolArray() => _cellContainer.ToBoolArray();

        public void SetCellAt(int x, int y, BoardCellType cell, bool needsUpdate = false)
        {
            _cellContainer.SetCellAt(x, y, cell);
            if (needsUpdate)
                BoardChanged?.Invoke(this, EventArgs.Empty);
        }

        public bool IsOutOfBounds(Point p) => _cellContainer.IsOutOfBounds(p);

        public static T TimeAction<T>(Func<T> func, out TimeSpan elapsed)
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
