using System.Collections.Generic;
using System.Linq;
using Quader.Engine.Collections;
using Quader.Engine.Replays;
using Quader.Engine.Settings;

namespace Quader.Engine;

public class AttackBuilder
{
    private readonly AttackSettings _attackSettings;
    private readonly Deque<int> _attackQueue = new();

    public IEnumerable<int> IncomingDamage => _attackQueue.ToArray();

    public AttackBuilder(AttackSettings settings)
    {
        _attackSettings = settings;
    }

    public void Attack(BoardHardDropInfo hardDropInfo)
    {
        Attack(CalculateAttack(hardDropInfo));
    }

    public void Attack(int attackLines)
    {
        if (attackLines > 0)
        {
            _attackQueue.AddBack(attackLines);
            //AttackReceived?.Invoke(this, attackLines);
        }
    }

    public int CalculateAttack(BoardHardDropInfo hardDropInfo)
    {
        int attack = _attackSettings.Lines0;

        if (hardDropInfo.LinesCleared == 0)
            return attack;

        var mods = hardDropInfo.Modificators;

        if (mods.HasFlag(BoardHardDropInfoModificators.Combo1))
            attack += _attackSettings.Combos[0];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo2))
            attack += _attackSettings.Combos[1];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo3))
            attack += _attackSettings.Combos[2];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo4))
            attack += _attackSettings.Combos[3];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo5))
            attack += _attackSettings.Combos[4];

        if (mods.HasFlag(BoardHardDropInfoModificators.AllClear))
            attack += _attackSettings.AllClear;

        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack1))
            attack += _attackSettings.BackToBacks[0];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack2))
            attack += _attackSettings.BackToBacks[1];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack3))
            attack += _attackSettings.BackToBacks[2];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack4))
            attack += _attackSettings.BackToBacks[3];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack5))
            attack += _attackSettings.BackToBacks[4];

        if (mods.HasFlag(BoardHardDropInfoModificators.TSpin))
        {
            if (mods.HasFlag(BoardHardDropInfoModificators.Single))
                attack += _attackSettings.TSpinSingle;
            if (mods.HasFlag(BoardHardDropInfoModificators.Double))
                attack += _attackSettings.TSpinDouble;
            if (mods.HasFlag(BoardHardDropInfoModificators.Triple))
                attack += _attackSettings.TSpinDouble;
        }
        else if (mods.HasFlag(BoardHardDropInfoModificators.TSpinMini))
        {
            if (mods.HasFlag(BoardHardDropInfoModificators.Single))
                attack += _attackSettings.TSpinSingleMini;
        }
        else
        {
            if (mods.HasFlag(BoardHardDropInfoModificators.Single))
                attack += _attackSettings.Lines1;
            if (mods.HasFlag(BoardHardDropInfoModificators.Double))
                attack += _attackSettings.Lines2;
            if (mods.HasFlag(BoardHardDropInfoModificators.Triple))
                attack += _attackSettings.Lines3;
            if (mods.HasFlag(BoardHardDropInfoModificators.Quad))
                attack += _attackSettings.Lines4;
        }

        return attack;
    }

    /*private void CreateBoardMoveType(ref BoardHardDropInfoModificators moveType, int linesCleared, Board.TSpinType tSpinType)
    {
        // All Clear - No pieces on the board after a hardDropInfo
        if (_piecesOnBoard == 0)
            moveType |= BoardHardDropInfoModificators.AllClear;

        // Basic clears - 4 lines is the max
        if (linesCleared == 1)
            moveType |= BoardHardDropInfoModificators.Single;
        else if (linesCleared == 2)
            moveType |= BoardHardDropInfoModificators.Double;
        else if (linesCleared == 3)
            moveType |= BoardHardDropInfoModificators.Triple;
        else if (linesCleared == 4)
        {
            moveType |= BoardHardDropInfoModificators.Quad;
            CurrentB2B++;
        }

        // Combo handling - the higher the combo - the bigger the spike
        if (CurrentCombo > 1)
            moveType |= BoardHardDropInfoModificators.Combo1;
        if (CurrentCombo >= 6)
            moveType |= BoardHardDropInfoModificators.Combo2;
        if (CurrentCombo >= 10)
            moveType |= BoardHardDropInfoModificators.Combo3;
        if (CurrentCombo >= 15)
            moveType |= BoardHardDropInfoModificators.Combo4;
        if (CurrentCombo >= 18)
            moveType |= BoardHardDropInfoModificators.Combo5;

        // If hardDropInfo does not contain a Quad, T-Spin or T-Spin Mini, B2B status is 0
        if (
            linesCleared > 0 &&
            !moveType.HasFlag(BoardHardDropInfoModificators.Quad) &&
            !moveType.HasFlag(BoardHardDropInfoModificators.TSpin) &&
            !moveType.HasFlag(BoardHardDropInfoModificators.TSpinMini)
        )
        {
            CurrentB2B = 0;
        }

        // Multiple successive T-Spins (including minis) or Quads
        if (CurrentB2B > 0)
            moveType |= BoardHardDropInfoModificators.BackToBack1;
        if (CurrentB2B >= 5)
            moveType |= BoardHardDropInfoModificators.BackToBack2;
        if (CurrentB2B >= 10)
            moveType |= BoardHardDropInfoModificators.BackToBack3;
        if (CurrentB2B >= 30)
            moveType |= BoardHardDropInfoModificators.BackToBack4;
        if (CurrentB2B >= 60)
            moveType |= BoardHardDropInfoModificators.BackToBack5;

        if (tSpinType != Board.TSpinType.None)
            CurrentB2B++;

        // Break the combo if cleared 0 lines
        if (linesCleared == 0)
            CurrentCombo = 0;
    }*/
}