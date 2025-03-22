using Quader.Engine.Pieces;

namespace Quader.Engine.Boards;

public enum GravityUpdateResult
{
    None,
    SoftDrop,
    HardDrop
}

public class GravityManager
{
    internal float CurrentGravity;
    internal float CurrentLock;
    internal float IntermediateY;
    internal bool YNeedsUpdate;
    internal int YToCheck;

    private readonly GravitySettings _gravitySettings;

    public bool IsEnabled { get; set; }

    public int LastSoftDropDiff { get; private set; }

    public GravityManager(GravitySettings gravitySettings)
    {
        _gravitySettings = gravitySettings;
        CurrentGravity = _gravitySettings.GravityBase;
        CurrentLock = _gravitySettings.LockDelay;
        IntermediateY = 0;
        YNeedsUpdate = true;
        YToCheck = 0;
        IsEnabled = true;
        LastSoftDropDiff = 0;
    }

    public void ProlongLock()
    {
        CurrentLock = Math.Min(CurrentLock + _gravitySettings.LockProlongAmount,
            _gravitySettings.LockDelay);
    }

    public void ResetLock()
    {
        CurrentLock = _gravitySettings.LockDelay;
    }

    public void Reset()
    {
        IntermediateY = 0;
        YNeedsUpdate = true;
        YToCheck = 0;

        CurrentGravity = _gravitySettings.GravityBase;
        CurrentLock = _gravitySettings.LockDelay;
    }

    public GravityUpdateResult Update(PieceManager pieceManager, TimeManager timeManager)
    {
        if (!IsEnabled) return GravityUpdateResult.None;

        var result = GravityUpdateResult.None;

        IntermediateY += CurrentGravity * timeManager.LastDt;

        if (YNeedsUpdate)
        {
            YToCheck = pieceManager.FindNearestY();
            YNeedsUpdate = false;
        }

        if (IntermediateY >= 1.0)
        {
            var diff = (int)Math.Max(IntermediateY - 1.0, 1.0);
            LastSoftDropDiff = diff;
            result = GravityUpdateResult.SoftDrop; // TODO: Add diff

            YNeedsUpdate = true;
            IntermediateY = 0;
        }

        if (YToCheck == pieceManager.CurrentPiece.Y)
            CurrentLock -= 1.0f * timeManager.LastDt;

        if (CurrentLock <= 0.0f)
        {
            result = GravityUpdateResult.HardDrop;
            CurrentLock = _gravitySettings.LockDelay;
            YNeedsUpdate = true;
            IntermediateY = 0;
        }

        CurrentGravity += _gravitySettings.GravityIncrease * timeManager.LastDt;

        return result;
    }
}