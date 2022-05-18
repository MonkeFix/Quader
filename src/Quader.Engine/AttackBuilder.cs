using System.Collections.Generic;
using System.Linq;
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

    public void Attack(BoardMove move)
    {
        Attack(CalculateAttack(move));
    }

    public void Attack(int attackLines)
    {
        if (attackLines > 0)
        {
            _attackQueue.AddBack(attackLines);
            //AttackReceived?.Invoke(this, attackLines);
        }
    }

    public int CalculateAttack(BoardMove move)
    {
        int attack = _attackSettings.Lines0;

        if (move.LinesCleared == 0)
            return attack;

        var mods = move.Modificators;

        if (mods.HasFlag(BoardMoveModificators.Combo1))
            attack += _attackSettings.Combos[0];
        if (mods.HasFlag(BoardMoveModificators.Combo2))
            attack += _attackSettings.Combos[1];
        if (mods.HasFlag(BoardMoveModificators.Combo3))
            attack += _attackSettings.Combos[2];
        if (mods.HasFlag(BoardMoveModificators.Combo4))
            attack += _attackSettings.Combos[3];
        if (mods.HasFlag(BoardMoveModificators.Combo5))
            attack += _attackSettings.Combos[4];

        if (mods.HasFlag(BoardMoveModificators.AllClear))
            attack += _attackSettings.AllClear;

        if (mods.HasFlag(BoardMoveModificators.BackToBack1))
            attack += _attackSettings.BackToBacks[0];
        if (mods.HasFlag(BoardMoveModificators.BackToBack2))
            attack += _attackSettings.BackToBacks[1];
        if (mods.HasFlag(BoardMoveModificators.BackToBack3))
            attack += _attackSettings.BackToBacks[2];
        if (mods.HasFlag(BoardMoveModificators.BackToBack4))
            attack += _attackSettings.BackToBacks[3];
        if (mods.HasFlag(BoardMoveModificators.BackToBack5))
            attack += _attackSettings.BackToBacks[4];

        if (mods.HasFlag(BoardMoveModificators.TSpin))
        {
            if (mods.HasFlag(BoardMoveModificators.Single))
                attack += _attackSettings.TSpinSingle;
            if (mods.HasFlag(BoardMoveModificators.Double))
                attack += _attackSettings.TSpinDouble;
            if (mods.HasFlag(BoardMoveModificators.Triple))
                attack += _attackSettings.TSpinDouble;
        }
        else if (mods.HasFlag(BoardMoveModificators.TSpinMini))
        {
            if (mods.HasFlag(BoardMoveModificators.Single))
                attack += _attackSettings.TSpinSingleMini;
        }
        else
        {
            if (mods.HasFlag(BoardMoveModificators.Single))
                attack += _attackSettings.Lines1;
            if (mods.HasFlag(BoardMoveModificators.Double))
                attack += _attackSettings.Lines2;
            if (mods.HasFlag(BoardMoveModificators.Triple))
                attack += _attackSettings.Lines3;
            if (mods.HasFlag(BoardMoveModificators.Quad))
                attack += _attackSettings.Lines4;
        }

        return attack;
    }

    /*private void CreateBoardMoveType(ref BoardMoveModificators moveType, int linesCleared, Board.TSpinType tSpinType)
    {
        // All Clear - No pieces on the board after a move
        if (_piecesOnBoard == 0)
            moveType |= BoardMoveModificators.AllClear;

        // Basic clears - 4 lines is the max
        if (linesCleared == 1)
            moveType |= BoardMoveModificators.Single;
        else if (linesCleared == 2)
            moveType |= BoardMoveModificators.Double;
        else if (linesCleared == 3)
            moveType |= BoardMoveModificators.Triple;
        else if (linesCleared == 4)
        {
            moveType |= BoardMoveModificators.Quad;
            CurrentB2B++;
        }

        // Combo handling - the higher the combo - the bigger the spike
        if (CurrentCombo > 1)
            moveType |= BoardMoveModificators.Combo1;
        if (CurrentCombo >= 6)
            moveType |= BoardMoveModificators.Combo2;
        if (CurrentCombo >= 10)
            moveType |= BoardMoveModificators.Combo3;
        if (CurrentCombo >= 15)
            moveType |= BoardMoveModificators.Combo4;
        if (CurrentCombo >= 18)
            moveType |= BoardMoveModificators.Combo5;

        // If move does not contain a Quad, T-Spin or T-Spin Mini, B2B status is 0
        if (
            linesCleared > 0 &&
            !moveType.HasFlag(BoardMoveModificators.Quad) &&
            !moveType.HasFlag(BoardMoveModificators.TSpin) &&
            !moveType.HasFlag(BoardMoveModificators.TSpinMini)
        )
        {
            CurrentB2B = 0;
        }

        // Multiple successive T-Spins (including minis) or Quads
        if (CurrentB2B > 0)
            moveType |= BoardMoveModificators.BackToBack1;
        if (CurrentB2B >= 5)
            moveType |= BoardMoveModificators.BackToBack2;
        if (CurrentB2B >= 10)
            moveType |= BoardMoveModificators.BackToBack3;
        if (CurrentB2B >= 30)
            moveType |= BoardMoveModificators.BackToBack4;
        if (CurrentB2B >= 60)
            moveType |= BoardMoveModificators.BackToBack5;

        if (tSpinType != Board.TSpinType.None)
            CurrentB2B++;

        // Break the combo if cleared 0 lines
        if (linesCleared == 0)
            CurrentCombo = 0;
    }*/
}