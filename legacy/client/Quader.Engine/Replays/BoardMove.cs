namespace Quader.Engine.Replays;

public class BoardMove
{
    public BoardHardDropInfo? HardDropInfo;

    public BoardMoveType Type;

    public double Tick;

    public ReplayMoveInfo? Info;
}

public class ReplayMoveInfo
{
    public int SoftDropFactor;
    public int MoveLeftFactor;
    public int MoveRightFactor;
}