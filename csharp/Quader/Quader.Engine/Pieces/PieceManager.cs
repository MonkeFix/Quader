using Quader.Engine.Boards;
using Quader.Engine.Pieces.WallKick;
using Quader.Engine.Primitives;
using Quader.Engine.Replays;
using Quader.Engine.Scoring;

namespace Quader.Engine.Pieces;

public class PieceManager
{
    public Piece CurrentPiece { get; private set; }
    private BoardSettings _boardSettings;
    public CellHolder CellHolder { get; }
    public PieceType? HoldPiece { get; private set; }
    public bool IsHoldUsed { get; private set; }
    public PieceQueue PieceQueue { get; }
    public bool IsEnabled { get; set; }
    private LastMoveType _lastMoveType;
    public int NearestY { get; private set; }

    public PieceManager(GameSettings gameSettings, int seed)
    {
        _boardSettings = gameSettings.Board;
        PieceQueue = new PieceQueue(seed);

        var nextPiece = PieceQueue.NextPiece();
        var piece = new Piece(nextPiece);
        ResetPiece(piece, _boardSettings.Width, _boardSettings.FullHeight);
        CurrentPiece = piece;

        CellHolder = new CellHolder(_boardSettings);
        NearestY = FindNearestY(CurrentPiece, CellHolder);
        _lastMoveType = LastMoveType.None;
        IsHoldUsed = false;
        IsEnabled = true;
        HoldPiece = null;
    }

    public BoardErrorReason TryCreatePiece(PieceType type, out Piece piece)
    {
        piece = new Piece(type);
        CurrentPiece = piece;
        ResetCurrentPiece();

        var adjusted = PieceHelpers.AdjustPositions(CurrentPiece.GetPoints(),
            new Point(CurrentPiece.X, CurrentPiece.Y));

        if (CellHolder.IntersectsAny(adjusted))
            return BoardErrorReason.CannotSpawnPiece;

        return BoardErrorReason.None;
    }

    public BoardErrorReason TryHoldPiece(out Piece piece)
    {
        piece = CurrentPiece;
        if (IsHoldUsed)
            return BoardErrorReason.None;

        if (!IsEnabled)
            return BoardErrorReason.BoardDisabled;

        IsHoldUsed = true;

        if (HoldPiece.HasValue)
        {
            var old = HoldPiece.Value;
            HoldPiece = CurrentPiece.PieceType;
            return TryCreatePiece(old, out piece);
        }
        else
        {
            HoldPiece = CurrentPiece.PieceType;
            var newPiece = PieceQueue.NextPiece();
            return TryCreatePiece(newPiece, out piece);
        }
    }

    public bool MoveLeft()
    {
        if (!IsEnabled) return false;

        if (TestMovement(-1, 0))
        {
            CurrentPiece.MoveLeft();
            _lastMoveType = LastMoveType.Movement;
            NearestY = FindNearestY();
            return true;
        }

        return false;
    }

    public void MoveLeftForce()
    {
        CurrentPiece.MoveLeft();
        _lastMoveType = LastMoveType.Movement;
        NearestY = FindNearestY();
    }

    public bool MoveRight()
    {
        if (!IsEnabled) return false;

        if (TestMovement(1, 0))
        {
            CurrentPiece.MoveRight();
            _lastMoveType = LastMoveType.Movement;
            NearestY = FindNearestY();
            return true;
        }

        return false;
    }

    public void MoveRightForce()
    {
        CurrentPiece.MoveRight();
        _lastMoveType = LastMoveType.Movement;
        NearestY = FindNearestY();
    }

    public bool Rotate(WallKickData wkd, RotationDirection rotation)
    {
        if (!IsEnabled) return false;

        var allTests = wkd.Get(CurrentPiece.WallKickType);
        var rotType = CurrentPiece.GetRotationType(rotation);
        var move = rotType.Move;
        var tests = allTests[move];

        var isSuccess = TestRotation(new WallKickCheckParams(tests, rotType.Points), out var point);
        if (isSuccess)
        {
            CurrentPiece.Rotate(rotation, point.X, point.Y);
            _lastMoveType = LastMoveType.Rotation;
            NearestY = FindNearestY();
            return true;
        }

        return false;
    }

    public bool SoftDrop()
    {
        if (!IsEnabled) return false;

        if (TestMovement(0, 1))
        {
            CurrentPiece.MoveDown();
            _lastMoveType = LastMoveType.Movement;
            NearestY = FindNearestY();
            return true;
        }

        return false;
    }

    public void SoftDropForce()
    {
        CurrentPiece.MoveDown();
        _lastMoveType = LastMoveType.Movement;
        NearestY = FindNearestY();
    }

    public BoardErrorReason HardDrop(out HardDropInfo info)
    {
        info = default;

        if (!IsEnabled) return BoardErrorReason.BoardDisabled;

        NearestY = FindNearestY();

        var tSpinStatus = TSpinStatus.None;
        if (CurrentPiece.PieceType == PieceType.T)
        {
            tSpinStatus = PieceHelpers.TestTOverhang(_boardSettings, CurrentPiece.X, CurrentPiece.Y,
                p => CellHolder.Intersects(p));
        }

        if (!TryApplyPiece(NearestY))
            return BoardErrorReason.CannotApplyPiece;

        var linesCleared = CellHolder.CheckRowClears(null);

        if (NearestY <= _boardSettings.Height && linesCleared.Count == 0)
            return BoardErrorReason.CannotApplyPiece;

        CellHolder.ClearRows(linesCleared);
        var linesClearedCount = linesCleared.Count;

        info.LinesCleared = linesClearedCount;
        info.TSpinStatus = tSpinStatus;
        info.LastMoveType = _lastMoveType;
        info.OccupiedCellsLeft = CellHolder.OccupiedCells;

        ResetCurrentPiece();
        IsHoldUsed = false;

        var nextPiece = PieceQueue.NextPiece();
        return TryCreatePiece(nextPiece, out _);
    }

    public void UpdateNearestY()
    {
        NearestY = FindNearestY();
    }

    public void Reset(int? newSeed)
    {
        IsHoldUsed = false;
        CellHolder.Clear();
        PieceQueue.Reset(newSeed);

        var nextPiece = PieceQueue.NextPiece();
        var piece = new Piece(nextPiece);
        ResetPiece(piece, _boardSettings.Width, _boardSettings.FullHeight);
        CurrentPiece = piece;

        _lastMoveType = LastMoveType.None;
        HoldPiece = null;

        Enable();

        NearestY = FindNearestY();
    }

    public void Enable() => IsEnabled = true;
    public void Disable() => IsEnabled = false;

    internal void RotateForce(RotationDirection rotation)
    {
        CurrentPiece.RotateSimple(rotation);
        NearestY = FindNearestY();
    }

    private void ResetCurrentPiece()
    {
        _lastMoveType = LastMoveType.None;
        ResetPiece(CurrentPiece, _boardSettings.Width, _boardSettings.FullHeight);
        NearestY = FindNearestY();
    }

    public int FindNearestY()
    {
        return FindNearestY(CurrentPiece, CellHolder);
    }

    private bool TestMovement(int x, int y)
    {
        var bounds = CurrentPiece.Bounds;

        if (bounds.X + x < 0 || bounds.X + bounds.Width + x > _boardSettings.Width)
            return false;
        if (bounds.Y + bounds.Height + y > _boardSettings.FullHeight)
            return false;

        var pos = CurrentPiece.GetPoints();
        var offset = new Point(CurrentPiece.X + x, CurrentPiece.Y + y);
        var newPos = PieceHelpers.AdjustPositions(pos, offset);

        return !CellHolder.IntersectsAny(newPos);
    }

    private bool TestRotation(WallKickCheckParams kickParams, out Point result)
    {
        result = new Point();

        foreach (var t in kickParams.Tests)
        {
            var test = new Point(t.X, -t.Y);
            var adjusted = PieceHelpers.AdjustPositions(kickParams.ExpectedPos,
                new Point(CurrentPiece.X + test.X, CurrentPiece.Y + test.Y));

            if (!CellHolder.IntersectsAny(adjusted))
            {
                result = test;
                return true;
            }
        }

        return false;
    }

    private bool TryApplyPiece(int y)
    {
        var points = CurrentPiece.GetPoints();
        var adjusted = PieceHelpers.AdjustPositions(points, new Point(CurrentPiece.X, y));

        var res = true;

        foreach (var point in adjusted)
        {
            if (CellHolder.IsOutOfBounds(point.X, point.Y)) continue;

            var cell = CellHolder.GetCellAt(point.X, point.Y);
            if (cell != CellType.None)
                res = false;

            var cellType = PieceHelpers.PieceTypeToCellType(CurrentPiece.PieceType);
            CellHolder.SetCellAt(point.X, point.Y, cellType);
        }

        return res;
    }

    private static void ResetPiece(Piece piece, int boardWidth, int boardHeight)
    {
        switch (piece.OffsetType)
        {
            case OffsetType.Cell:
                piece.X = boardWidth / 2 - 1;
                break;
            case OffsetType.BetweenCells:
                piece.X = (int)Math.Round((boardWidth / 2f));
                break;
        }

        switch (piece.PieceType)
        {
            case PieceType.I:
                piece.Y = boardHeight / 2 + 1;
                break;
            default:
                piece.Y = boardHeight / 2;
                break;
        }

        piece.Reset();
    }

    private static int FindNearestY(Piece piece, CellHolder cellHolder)
    {
        var y = piece.Y;
        var points = piece.GetPoints();

        for (int i = piece.Y; i <= cellHolder.Height; i++)
        {
            var offset = new Point(piece.X, i);
            var newPoints = PieceHelpers.AdjustPositions(points, offset);
            if (cellHolder.IntersectsAny(newPoints))
                break;

            y = i;
        }

        return y;
    }
}