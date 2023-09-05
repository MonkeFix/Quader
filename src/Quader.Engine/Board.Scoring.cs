using System.Drawing;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;

namespace Quader.Engine;

public partial class Board
{
    // ReSharper disable once InconsistentNaming
    enum TSpinType
    {
        None, Full, Mini
    }


    public Point[] PointsChecked = new Point[4];


    public int CalculateAttack(BoardHardDropInfo hardDropInfo)
    {
        int attack = AttackSettings.Lines0;

        if (hardDropInfo.LinesCleared == 0)
            return attack;

        var mods = hardDropInfo.Modificators;

        if (mods.HasFlag(BoardHardDropInfoModificators.Combo1))
            attack += AttackSettings.Combos[0];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo2))
            attack += AttackSettings.Combos[1];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo3))
            attack += AttackSettings.Combos[2];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo4))
            attack += AttackSettings.Combos[3];
        if (mods.HasFlag(BoardHardDropInfoModificators.Combo5))
            attack += AttackSettings.Combos[4];

        if (mods.HasFlag(BoardHardDropInfoModificators.AllClear))
            attack += AttackSettings.AllClear;

        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack1))
            attack += AttackSettings.BackToBacks[0];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack2))
            attack += AttackSettings.BackToBacks[1];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack3))
            attack += AttackSettings.BackToBacks[2];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack4))
            attack += AttackSettings.BackToBacks[3];
        if (mods.HasFlag(BoardHardDropInfoModificators.BackToBack5))
            attack += AttackSettings.BackToBacks[4];

        if (mods.HasFlag(BoardHardDropInfoModificators.TSpin))
        {
            if (mods.HasFlag(BoardHardDropInfoModificators.Single))
                attack += AttackSettings.TSpinSingle;
            if (mods.HasFlag(BoardHardDropInfoModificators.Double))
                attack += AttackSettings.TSpinDouble;
            if (mods.HasFlag(BoardHardDropInfoModificators.Triple))
                attack += AttackSettings.TSpinDouble;
        }
        else if (mods.HasFlag(BoardHardDropInfoModificators.TSpinMini))
        {
            if (mods.HasFlag(BoardHardDropInfoModificators.Single))
                attack += AttackSettings.TSpinSingleMini;
        }
        else
        {
            if (mods.HasFlag(BoardHardDropInfoModificators.Single))
                attack += AttackSettings.Lines1;
            if (mods.HasFlag(BoardHardDropInfoModificators.Double))
                attack += AttackSettings.Lines2;
            if (mods.HasFlag(BoardHardDropInfoModificators.Triple))
                attack += AttackSettings.Lines3;
            if (mods.HasFlag(BoardHardDropInfoModificators.Quad))
                attack += AttackSettings.Lines4;
        }

        return attack;
    }

    private void CreateBoardMoveType(ref BoardHardDropInfoModificators hardDropInfoType, int linesCleared, TSpinType tSpinType)
    {
        // All Clear - No pieces on the board after a hardDropInfo
        if (_piecesOnBoard == 0)
            hardDropInfoType |= BoardHardDropInfoModificators.AllClear;

        // Basic clears - 4 lines is the max
        if (linesCleared == 1)
            hardDropInfoType |= BoardHardDropInfoModificators.Single;
        else if (linesCleared == 2)
            hardDropInfoType |= BoardHardDropInfoModificators.Double;
        else if (linesCleared == 3)
            hardDropInfoType |= BoardHardDropInfoModificators.Triple;
        else if (linesCleared == 4)
        {
            hardDropInfoType |= BoardHardDropInfoModificators.Quad;
            CurrentB2B++;
        }

        // Combo handling - the higher the combo - the bigger the spike
        if (CurrentCombo > 1)
            hardDropInfoType |= BoardHardDropInfoModificators.Combo1;
        if (CurrentCombo >= 6)
            hardDropInfoType |= BoardHardDropInfoModificators.Combo2;
        if (CurrentCombo >= 10)
            hardDropInfoType |= BoardHardDropInfoModificators.Combo3;
        if (CurrentCombo >= 15)
            hardDropInfoType |= BoardHardDropInfoModificators.Combo4;
        if (CurrentCombo >= 18)
            hardDropInfoType |= BoardHardDropInfoModificators.Combo5;

        // If hardDropInfo does not contain a Quad, T-Spin or T-Spin Mini, B2B status is 0
        if (
            linesCleared > 0 &&
            !hardDropInfoType.HasFlag(BoardHardDropInfoModificators.Quad) &&
            !hardDropInfoType.HasFlag(BoardHardDropInfoModificators.TSpin) &&
            !hardDropInfoType.HasFlag(BoardHardDropInfoModificators.TSpinMini)
        )
        {
            CurrentB2B = 0;
        }

        // Multiple successive T-Spins (including minis) or Quads
        if (CurrentB2B > 0)
            hardDropInfoType |= BoardHardDropInfoModificators.BackToBack1;
        if (CurrentB2B >= 5)
            hardDropInfoType |= BoardHardDropInfoModificators.BackToBack2;
        if (CurrentB2B >= 10)
            hardDropInfoType |= BoardHardDropInfoModificators.BackToBack3;
        if (CurrentB2B >= 30)
            hardDropInfoType |= BoardHardDropInfoModificators.BackToBack4;
        if (CurrentB2B >= 60)
            hardDropInfoType |= BoardHardDropInfoModificators.BackToBack5;

        if (tSpinType != TSpinType.None)
            CurrentB2B++;

        // Break the combo if cleared 0 lines
        if (linesCleared == 0)
            CurrentCombo = 0;
    }

    private TSpinType CheckTOverhang()
    {
        // Safety check
        if (CurrentPiece.Type != PieceType.T)
            return TSpinType.None;

        // A block must be in any of the blocks in this graph:
        // #T#
        // TTT
        // #T#
        // T - T Piece
        // # - Block

        var pieceX = CurrentPiece.X;
        var pieceY = CurrentPiece.Y;

        var topLeft = new Point(pieceX - 1, pieceY - 1);
        var topRight = new Point(pieceX + 1, pieceY - 1);
        var bottomLeft = new Point(pieceX - 1, pieceY + 1);
        var bottomRight = new Point(pieceX + 1, pieceY + 1);

        var pArr = new[] { topLeft, topRight, bottomLeft, bottomRight };

        PointsChecked = pArr;

        var oobOverhangs = 0; // out of bounds
        var nonOobOverhangs = 0;

        foreach (var p in pArr)
        {
            if (IsOutOfBounds(p))
                oobOverhangs++;
            else
            {
                var cell = GetCellAt(p.X, p.Y);
                if (cell != BoardCellType.None)
                    nonOobOverhangs++;
            }
        }

        // We cannot have more than 4 blocks checked in any case!
        // Insist.IsFalse(oobOverhangs + nonOobOverhangs > 4);

        if (oobOverhangs > 0 && nonOobOverhangs > 0)
            return TSpinType.Mini;

        if (oobOverhangs == 0 && nonOobOverhangs >= 3)
            return TSpinType.Full;

        return TSpinType.None;
    }
}