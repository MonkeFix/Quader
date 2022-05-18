using Microsoft.Xna.Framework;
using Nez;
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


    public int CalculateAttack(BoardMove move)
    {
        int attack = AttackSettings.Lines0;

        if (move.LinesCleared == 0)
            return attack;

        var mods = move.Modificators;

        if (mods.HasFlag(BoardMoveModificators.Combo1))
            attack += AttackSettings.Combos[0];
        if (mods.HasFlag(BoardMoveModificators.Combo2))
            attack += AttackSettings.Combos[1];
        if (mods.HasFlag(BoardMoveModificators.Combo3))
            attack += AttackSettings.Combos[2];
        if (mods.HasFlag(BoardMoveModificators.Combo4))
            attack += AttackSettings.Combos[3];
        if (mods.HasFlag(BoardMoveModificators.Combo5))
            attack += AttackSettings.Combos[4];

        if (mods.HasFlag(BoardMoveModificators.AllClear))
            attack += AttackSettings.AllClear;

        if (mods.HasFlag(BoardMoveModificators.BackToBack1))
            attack += AttackSettings.BackToBacks[0];
        if (mods.HasFlag(BoardMoveModificators.BackToBack2))
            attack += AttackSettings.BackToBacks[1];
        if (mods.HasFlag(BoardMoveModificators.BackToBack3))
            attack += AttackSettings.BackToBacks[2];
        if (mods.HasFlag(BoardMoveModificators.BackToBack4))
            attack += AttackSettings.BackToBacks[3];
        if (mods.HasFlag(BoardMoveModificators.BackToBack5))
            attack += AttackSettings.BackToBacks[4];

        if (mods.HasFlag(BoardMoveModificators.TSpin))
        {
            if (mods.HasFlag(BoardMoveModificators.Single))
                attack += AttackSettings.TSpinSingle;
            if (mods.HasFlag(BoardMoveModificators.Double))
                attack += AttackSettings.TSpinDouble;
            if (mods.HasFlag(BoardMoveModificators.Triple))
                attack += AttackSettings.TSpinDouble;
        }
        else if (mods.HasFlag(BoardMoveModificators.TSpinMini))
        {
            if (mods.HasFlag(BoardMoveModificators.Single))
                attack += AttackSettings.TSpinSingleMini;
        }
        else
        {
            if (mods.HasFlag(BoardMoveModificators.Single))
                attack += AttackSettings.Lines1;
            if (mods.HasFlag(BoardMoveModificators.Double))
                attack += AttackSettings.Lines2;
            if (mods.HasFlag(BoardMoveModificators.Triple))
                attack += AttackSettings.Lines3;
            if (mods.HasFlag(BoardMoveModificators.Quad))
                attack += AttackSettings.Lines4;
        }

        return attack;
    }

    private void CreateBoardMoveType(ref BoardMoveModificators moveType, int linesCleared, TSpinType tSpinType)
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
        Insist.IsFalse(oobOverhangs + nonOobOverhangs > 4);

        if (oobOverhangs > 0 && nonOobOverhangs > 0)
            return TSpinType.Mini;

        if (oobOverhangs == 0 && nonOobOverhangs >= 3)
            return TSpinType.Full;

        return TSpinType.None;
    }
}