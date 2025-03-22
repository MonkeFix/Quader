using Quader.Engine.Garbage;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.WallKick;
using Quader.Engine.Replays;
using Quader.Engine.Scoring;

namespace Quader.Engine.Boards;

public class Board
{
    private GameSettings _gameSettings;
    GravityManager _gravityManager;
    private bool _isEnabled = true;

    public bool IsEnabled
    {
        get => _isEnabled;
        set
        {
            if (value) Enable();
            else Disable();
        }
    }

    private WallKickData _wallKickData;
    private ScoringManager _scoringManager;
    private PieceManager _pieceManager;
    BoardStats _boardStats;
    public bool IsDead { get; private set; }
    GarbageManager _garbageManager;
    private ReplayManager _replayManager;
    private float _curSec;

    public Board(GameSettings gameSettings, WallKickData wallKickData, int seed)
    {
        _gameSettings = gameSettings;
        IsEnabled = true;
        _wallKickData = wallKickData;
        _scoringManager = new ScoringManager();
        _boardStats = new BoardStats();
        IsDead = false;
        _garbageManager = new GarbageManager(_gameSettings.Attack);
        _curSec = 0;
        _replayManager = new ReplayManager();

        _gravityManager = new GravityManager(_gameSettings.Gravity);
        _pieceManager = new PieceManager(_gameSettings, seed);
    }

    public BoardErrorReason Update(TimeManager timeManager, out MoveResult? moveResult)
    {
        moveResult = null;
        if (!IsEnabled) return BoardErrorReason.BoardDisabled;
        if (IsDead) return BoardErrorReason.BoardDead;

        _curSec = timeManager.ElapsedSeconds;

        _boardStats.Update(timeManager);
        _garbageManager.Update(timeManager);

        var res = _gravityManager.Update(_pieceManager, timeManager);

        switch (res)
        {
            case GravityUpdateResult.None:
                return BoardErrorReason.None;
            case GravityUpdateResult.SoftDrop:
                SoftDrop(_gravityManager.LastSoftDropDiff);
                return BoardErrorReason.None;
            case GravityUpdateResult.HardDrop:
                return HardDrop(out moveResult);
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    public int MoveLeft(int delta)
    {
        var moveCount = 0;

        for (int i = 0; i < delta; i++)
        {
            if (_pieceManager.MoveLeft())
            {
                _replayManager.AddMove(_curSec, MoveAction.MoveLeft);
                moveCount++;
            }
        }

        return moveCount;
    }

    public int MoveRight(int delta)
    {
        var moveCount = 0;

        for (int i = 0; i < delta; i++)
        {
            if (_pieceManager.MoveRight())
            {
                _replayManager.AddMove(_curSec, MoveAction.MoveRight);
                moveCount++;
            }
        }

        return moveCount;
    }

    public bool TryRotate(RotationDirection direction, out RotationState state)
    {
        if (_pieceManager.Rotate(_wallKickData, direction))
        {
            _gravityManager.ProlongLock();

            var action = direction switch
            {
                RotationDirection.Clockwise => MoveAction.RotateCW,
                RotationDirection.CounterClockwise => MoveAction.RotateCCW,
                RotationDirection.Deg180 => MoveAction.RotateDeg180,
                _ => throw new ArgumentOutOfRangeException(nameof(direction), direction, null)
            };
            _replayManager.AddMove(_curSec, action);

            state = _pieceManager.CurrentPiece.CurrentRotation;
            return true;
        }

        state = _pieceManager.CurrentPiece.CurrentRotation;
        return false;
    }

    public BoardErrorReason TryHoldPiece(out Piece piece)
    {
        var result = _pieceManager.TryHoldPiece(out piece);

        if (result == BoardErrorReason.None)
            _replayManager.AddMove(_curSec, MoveAction.HoldPiece);

        return result;
    }

    public PieceType? GetHoldPiece()
    {
        return _pieceManager.HoldPiece;
    }

    public BoardErrorReason HardDrop(out MoveResult? moveResult)
    {
        moveResult = null;

        if (!IsEnabled) return BoardErrorReason.BoardDisabled;
        if (IsDead) return BoardErrorReason.BoardDead;

        var pieceHardDropResult = _pieceManager.HardDrop(out var info);
        if (pieceHardDropResult != BoardErrorReason.None) return pieceHardDropResult;

        _scoringManager.HardDrop(info);
        _replayManager.AddMove(_curSec, MoveAction.HardDrop);

        var moveQueue = _replayManager.EndMove();
        moveResult = new MoveResult(_scoringManager, info, _gameSettings.Attack, _garbageManager,
            _pieceManager.CellHolder, moveQueue, _curSec);

        var totalDamage = moveResult.Value.Attack.OutDamage;

        _boardStats.HardDrop(ref info, _scoringManager, totalDamage);

        foreach (var dmg in moveResult.Value.Attack.InDamageQueue)
        {
            _garbageManager.PushGarbageAt(dmg.Amount, (int)dmg.HoleX, _pieceManager.CellHolder);
            _pieceManager.UpdateNearestY();
        }

        return BoardErrorReason.None;
    }

    public int SoftDrop(int delta)
    {
        var dt = Math.Min(delta, _gameSettings.Board.FullHeight);
        var amountMoved = 0;

        for (int i = 0; i < dt; i++)
        {
            if (_pieceManager.SoftDrop())
            {
                _gravityManager.ResetLock();
                _replayManager.AddMove(_curSec, MoveAction.SoftDrop);
                amountMoved++;
            }
        }

        return amountMoved;
    }

    public void PushGarbage(int amount, int messiness) =>
        _garbageManager.PushGarbage(amount, messiness, _pieceManager.CellHolder);

    public void Attack(int damage) => _garbageManager.Attack(_gameSettings.Board.Width, damage);

    public int FindNearestY() => _pieceManager.FindNearestY();

    public void Reset(int? newSeed)
    {
        _gravityManager.Reset();
        _scoringManager.Reset();
        _boardStats.Reset();
        _pieceManager.Reset(newSeed);
        IsDead = false;
        Enable();
        _replayManager.Reset();
        _garbageManager.Reset();
    }

    public void Enable()
    {
        if (IsEnabled) return;

        _isEnabled = true;
        _gravityManager.IsEnabled = true;
        _pieceManager.IsEnabled = true;
    }

    public void Disable()
    {
        if (!IsEnabled) return;

        _isEnabled = false;
        _gravityManager.IsEnabled = false;
        _pieceManager.IsEnabled = false;
    }
}