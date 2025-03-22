using Quader.Engine.Scoring;

namespace Quader.Engine.Replays;

public class BoardStats
{
    public float ElapsedSeconds;
    public float Apm;
    public float Pps;
    public int TotalPieces;

    public int Singles;
    public int Doubles;
    public int Triples;
    public int Quads;
    public int TSpins;
    public int TSpinMinis;
    public int TSpinSingles;
    public int TSpinDoubles;
    public int TSpinTriples;
    public int AllClears;
    public int MaxCombo;
    public int MaxB2b;

    public int TotalDamage;

    public void Update(TimeManager timeManager)
    {
        ElapsedSeconds = timeManager.ElapsedSeconds;
        if (ElapsedSeconds > 0)
            Pps = TotalPieces / ElapsedSeconds;

        if (ElapsedSeconds > 0)
            Apm = TotalDamage / (ElapsedSeconds / 60f);
    }

    public void HardDrop(ref HardDropInfo info, ScoringManager scoringManager, int damage)
    {
        TotalPieces++;

        if (info.LinesCleared == 1)
            Singles++;
        else if (info.LinesCleared == 2)
            Doubles++;
        else if (info.LinesCleared == 3)
            Triples++;
        else if (info.LinesCleared == 4)
            Quads++;

        switch (info.TSpinStatus)
        {
            case TSpinStatus.Full:
                TSpins++;
                if (info.LinesCleared == 1)
                    TSpinSingles++;
                else if (info.LinesCleared == 2)
                    TSpinDoubles++;
                else if (info.LinesCleared == 3)
                    TSpinTriples++;
                break;
            case TSpinStatus.Mini:
                TSpinMinis++;
                if (info.LinesCleared == 1)
                    TSpinSingles++;
                else if (info.LinesCleared == 2)
                    TSpinDoubles++;
                break;
            case TSpinStatus.None:
            default:
                break;
        }

        if (info.OccupiedCellsLeft == 0)
            AllClears++;

        if (damage > 0)
            TotalDamage += damage;

        MaxB2b = Math.Max(MaxB2b, scoringManager.B2B);
        MaxCombo = Math.Max(MaxCombo, scoringManager.Combo);
    }

    public void Reset()
    {
        ElapsedSeconds = 0;
        Apm = 0;
        Pps = 0;
        TotalPieces = 0;
        TotalDamage = 0;

        Singles = 0;
        Doubles = 0;
        Triples = 0;
        Quads = 0;
        TSpins = 0;
        TSpinMinis = 0;
        TSpinSingles = 0;
        TSpinDoubles = 0;
        TSpinTriples = 0;
        AllClears = 0;
        MaxCombo = 0;
        MaxB2b = 0;
    }
}