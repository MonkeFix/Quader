using Microsoft.Xna.Framework;
using Nez;
using Quader.Engine.Pieces;

namespace Quader.Engine;

public partial class Board
{
    // ReSharper disable once InconsistentNaming
    enum TSpinType
    {
        None, Full, Mini
    }


    public Point[] PointsChecked = new Point[4];

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