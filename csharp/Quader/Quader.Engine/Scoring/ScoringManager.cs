using Quader.Engine.Replays;

namespace Quader.Engine.Scoring;

public class ScoringManager
{
    public int Combo { get; private set; }
    public int B2B { get; private set; }

    public void HardDrop(HardDropInfo info)
    {
        if (info.LinesCleared == 4 || (info.LastMoveType == LastMoveType.Rotation &&
                                       (info.LinesCleared >= 1 &&
                                        info.TSpinStatus != TSpinStatus.None)))
        {
            B2B++;
        }
        else if (info.LinesCleared != 0)
        {
            B2B = 0;
        }

        if (info.LinesCleared > 0)
            Combo++;
        else
            Combo = 0;
    }

    public void Reset()
    {
        Combo = 0;
        B2B = 0;
    }
}