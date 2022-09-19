using Nez.Persistence;
using Nez.Persistence.Binary;

namespace Quader.Engine.Replays;

public class BoardMove : IPersistable
{
    public BoardHardDropInfo? HardDropInfo;

    public BoardMoveType Type;

    public double Tick;

    public ReplayMoveInfo? Info;

    public void Recover(IPersistableReader reader)
    {
        var hbm = reader.ReadBool();
        if (hbm)
        {
            HardDropInfo = new BoardHardDropInfo();
            reader.ReadPersistableInto(HardDropInfo);
        }
        Type = (BoardMoveType)reader.ReadInt();
        Tick = reader.ReadDouble();
        var hi = reader.ReadBool();
        if (hi)
        {
            Info = new ReplayMoveInfo();
            reader.ReadPersistableInto(Info);
        }
    }

    public void Persist(IPersistableWriter writer)
    {
        var hbm = HardDropInfo != null;
        writer.Write(hbm);
        if (hbm)
            writer.Write(HardDropInfo);
        writer.Write((int)Type);
        writer.Write(Tick);
        var hi = Info != null;
        writer.Write(hi);
        if (hi)
            writer.Write(Info);
    }
}

public class ReplayMoveInfo : IPersistable
{
    public int SoftDropFactor;
    public int MoveLeftFactor;
    public int MoveRightFactor;

    public void Recover(IPersistableReader reader)
    {
        SoftDropFactor = reader.ReadInt();
        MoveLeftFactor = reader.ReadInt();
        MoveRightFactor = reader.ReadInt();
    }

    public void Persist(IPersistableWriter writer)
    {
        writer.Write(SoftDropFactor);
        writer.Write(MoveLeftFactor);
        writer.Write(MoveRightFactor);
    }
}