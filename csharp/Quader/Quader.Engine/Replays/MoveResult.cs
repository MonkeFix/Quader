using Quader.Engine.Boards;
using Quader.Engine.Garbage;
using Quader.Engine.Scoring;

namespace Quader.Engine.Replays;

public struct MoveResult
{
    public float Timestamp;
    public uint ModBits;
    public int B2B;
    public int Combo;
    public bool IsSuccess;
    public GarbageHardDropResult Attack;

    public HardDropInfo HardDropInfo;
    public IEnumerable<ReplayMove> MoveQueue;

    public MoveResult(
        ScoringManager scoringManager,
        HardDropInfo info,
        AttackSettings attackSettings,
        GarbageManager garbageManager,
        CellHolder cellHolder,
        IEnumerable<ReplayMove> moveQueue,
        float curSec)
    {
        IsSuccess = true;
        B2B = scoringManager.B2B;
        Combo = scoringManager.Combo;
        HardDropInfo = info;
        MoveQueue = moveQueue;
        Timestamp = curSec;

        var bits = AttackCalculator.CreateBoardMoveBits(cellHolder.OccupiedCells, ref this,
            HardDropInfo.TSpinStatus);
        ModBits = bits;

        var dmg = AttackCalculator.CalculateDamage(ref attackSettings, ref this);
        Attack = garbageManager.HardDrop(HardDropInfo.LinesCleared, (int)dmg);
    }
}