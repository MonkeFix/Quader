using System;
using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;

namespace Quader.Engine;

public partial class Board
{
    /// <summary>
    /// Moves the current piece left by specified delta
    /// </summary>
    /// <param name="delta">Move left delta offset. Must be positive</param>
    public void PieceMoveLeft(int delta = 1)
    {
        delta = Math.Min(delta, Width);

        for (int i = 0; i < delta; i++)
        {
            if (TestMovement(-1, 0))
            {
                CurrentPiece.X -= 1;
                LastMoveType = LastMoveType.Movement;

                PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(-1, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
            }
        }
    }

    /// <summary>
    /// Moves the current piece right by specified delta
    /// </summary>
    /// <param name="delta">Move right delta offset. Must be positive</param>
    public void PieceMoveRight(int delta = 1)
    {
        delta = Math.Min(delta, Width);

        for (int i = 0; i < delta; i++)
        {
            if (TestMovement(1, 0))
            {
                CurrentPiece.X += 1;
                LastMoveType = LastMoveType.Movement;

                PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(1, 0), new Point(CurrentPiece.X, CurrentPiece.Y)));
            }
        }
    }

    public void Rotate(Rotation rotation)
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

        PieceRotated?.Invoke(this, CurrentPiece);
    }


    public void ResetPiece(PieceBase piece)
    {
        _intermediateY = 0;
        piece.CurrentRotation = PieceStartPosition.Initial;

        if (piece.OffsetType == OffsetType.BetweenCells)
            piece.X = Width / 2;
        else
            piece.X = (int)Math.Round((Width - 1) / 2.0);

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

        delta = Math.Min(delta, _yToCheck);

        for (int i = 0; i < delta; i++)
        {
            if (TestMovement(0, 1))
                CurrentPiece.Y += 1;
            else
                res = false;
        }

        PieceMoved?.Invoke(this, new PieceMovedEventArgs(new Point(0, delta), new Point(CurrentPiece.X, CurrentPiece.Y)));

        _yNeedsUpdate = true;

        return res;
    }

    public BoardMove HardDrop()
    {
        var nearestY = FindNearestY();

        if (!TryApplyPiece(CurrentPiece.CurrentPos, CurrentPiece.X, nearestY))
        {
            PieceCannotBeSpawned?.Invoke(this, EventArgs.Empty);
            return new BoardMove();
        }

        var linesCleared = _cellContainer.CheckLineClears(CurrentPiece.Bounds);

        if (nearestY <= Height && LastMove.LinesCleared == 0 && linesCleared.Length == 0)
        {
            PieceCannotBeSpawned?.Invoke(this, EventArgs.Empty);
            return new BoardMove();
        }

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

        _cellContainer.ClearLines(linesCleared);
        if (linesCleared.Length > 0)
        {
            LinesCleared?.Invoke(this, linesCleared.Length);
        }

        CreateBoardMoveType(ref moveType, linesCleared.Length, tSpinType);

        LastMoveType = LastMoveType.None;

        var bm = new BoardMove
        {
            LinesCleared = linesCleared.Length,
            Modificators = moveType,
            Timestamp = DateTime.UtcNow,
            BackToBack = CurrentB2B,
            Combo = CurrentCombo++,
            Success = true,
            Attack = 0
        };

        if (_attackQueue.Count > 0)
        {
            if (bm.LinesCleared > 0)
            {
                var damageCancel = CalculateAttack(bm);
                var res = false;

                while (_attackQueue.Count > 0)
                {
                    var attack = _attackQueue.RemoveFront();
                    damageCancel -= attack;

                    if (damageCancel <= 0)
                    {
                        if (damageCancel != 0)
                            _attackQueue.AddFront(-damageCancel);
                        res = true;
                        break;
                    }
                }

                if (!res)
                    bm.Attack = Math.Max(0, damageCancel);
            }
            else
            {
                PushGarbage(_attackQueue.RemoveFront());
            }
        }
        else
        {
            bm.Attack = CalculateAttack(bm);
        }

        LastMove = bm;

        PieceHardDropped?.Invoke(this, bm);
        BoardChanged?.Invoke(this, EventArgs.Empty);
        ResetPiece(CurrentPiece);

        return bm;
    }

    public bool TestMovement(int xOffset, int yOffset)
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
        var adjusted = BoardUtils.AdjustPositions(points, new Point(x, y));

        bool res = true;

        foreach (var point in adjusted)
        {
            var piece = GetCellAt(point.X, point.Y);

            if (piece != BoardCellType.None)
                res = false;
            SetCellAt(point.X, point.Y, CurrentPiece.BoardCellType);
            _piecesOnBoard++;
        }

        BoardChanged?.Invoke(this, EventArgs.Empty);

        return res;
    }
}