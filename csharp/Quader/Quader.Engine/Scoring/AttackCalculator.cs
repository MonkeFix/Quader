using Quader.Engine.Replays;

namespace Quader.Engine.Scoring;

public static class AttackCalculator
{
    public static uint CreateBoardMoveBits(int totalCells, ref MoveResult moveResult,
        TSpinStatus tSpinStatus)
    {
        DamageMods res = 0;

        if (totalCells == 0)
            res |= DamageMods.AllClear;

        var lc = moveResult.HardDropInfo.LinesCleared;
        if (lc == 1)
            res |= DamageMods.Single;
        else if (lc == 2)
            res |= DamageMods.Double;
        else if (lc == 3)
            res |= DamageMods.Triple;
        else if (lc == 4)
            res |= DamageMods.Quad;

        if (moveResult.Combo > Thresholds.Combo5)
            res |= DamageMods.Combo5;
        else if (moveResult.Combo > Thresholds.Combo4)
            res |= DamageMods.Combo4;
        else if (moveResult.Combo > Thresholds.Combo3)
            res |= DamageMods.Combo3;
        else if (moveResult.Combo > Thresholds.Combo2)
            res |= DamageMods.Combo2;
        else if (moveResult.Combo > Thresholds.Combo1)
            res |= DamageMods.Combo1;

        switch (tSpinStatus)
        {
            case TSpinStatus.Full:
                res |= DamageMods.TSpinFull;
                break;
            case TSpinStatus.Mini:
                res |= DamageMods.TSpinMini;
                break;
            case TSpinStatus.None:
            default:
                break;
        }

        if (moveResult.B2B > Thresholds.B2B5)
            res |= DamageMods.B2B5;
        if (moveResult.B2B > Thresholds.B2B4)
            res |= DamageMods.B2B4;
        if (moveResult.B2B > Thresholds.B2B3)
            res |= DamageMods.B2B3;
        if (moveResult.B2B > Thresholds.B2B2)
            res |= DamageMods.B2B2;
        if (moveResult.B2B > Thresholds.B2B1)
            res |= DamageMods.B2B1;

        return (uint)res;
    }

    public static uint CalculateDamage(ref AttackSettings attackSettings, ref MoveResult moveResult)
    {
        var attack = attackSettings.Lines0;

        if (moveResult.HardDropInfo.LinesCleared == 0) return attack;

        var mods = moveResult.ModBits;

        if (HasFlag(mods, DamageMods.Combo1))
            attack += attackSettings.Combos[0];
        if (HasFlag(mods, DamageMods.Combo2))
            attack += attackSettings.Combos[1];
        if (HasFlag(mods, DamageMods.Combo3))
            attack += attackSettings.Combos[2];
        if (HasFlag(mods, DamageMods.Combo4))
            attack += attackSettings.Combos[3];
        if (HasFlag(mods, DamageMods.Combo5))
            attack += attackSettings.Combos[4];

        if (HasFlag(mods, DamageMods.AllClear))
            attack += attackSettings.AllClear;

        if (HasFlag(mods, DamageMods.B2B1))
            attack += attackSettings.B2Bs[0];
        if (HasFlag(mods, DamageMods.B2B2))
            attack += attackSettings.B2Bs[1];
        if (HasFlag(mods, DamageMods.B2B3))
            attack += attackSettings.B2Bs[2];
        if (HasFlag(mods, DamageMods.B2B4))
            attack += attackSettings.B2Bs[3];
        if (HasFlag(mods, DamageMods.B2B5))
            attack += attackSettings.B2Bs[4];

        if (HasFlag(mods, DamageMods.TSpinFull))
        {
            if (HasFlag(mods, DamageMods.Single))
                attack += attackSettings.TSpinSingle;
            if (HasFlag(mods, DamageMods.Double))
                attack += attackSettings.TSpinDouble;
            if (HasFlag(mods, DamageMods.Triple))
                attack += attackSettings.TSpinTriple;
        }
        else if (HasFlag(mods, DamageMods.TSpinMini))
        {
            if (HasFlag(mods, DamageMods.Single))
                attack += attackSettings.TSpinSingleMini;
        }
        else
        {
            if (HasFlag(mods, DamageMods.Single))
                attack += attackSettings.Lines1;
            if (HasFlag(mods, DamageMods.Double))
                attack += attackSettings.Lines2;
            if (HasFlag(mods, DamageMods.Triple))
                attack += attackSettings.Lines3;
            if (HasFlag(mods, DamageMods.Quad))
                attack += attackSettings.Lines4;
        }

        return attack;
    }

    private static bool HasFlag(uint data, DamageMods mod)
    {
        return (data & (uint)mod) != 0;
    }
}