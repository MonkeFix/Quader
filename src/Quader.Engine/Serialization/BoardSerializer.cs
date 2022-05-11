using System;

namespace Quader.Engine.Serialization;

public static class BoardSerializer
{
    public static BoardEncoding Serialize(this Board board)
    {
        var res = "";

        for (int y = 0; y < board.TotalHeight; y++)
        {
            for (int x = 0; x < board.Width; x++)
            {
                char cellCh;
                var c = board.GetCellAt(x, y);

                if (c == BoardCellType.None)
                    cellCh = ' ';
                else if (c == BoardCellType.Garbage)
                    cellCh = 'X';
                else if (c == BoardCellType.Solid)
                    cellCh = '#';
                else
                    cellCh = c.ToString()[0];

                res += cellCh;
            }
        }

        return new BoardEncoding
        {
            Code = res,
            Height = board.Height,
            Width = board.Width,
            TotalHeight = board.TotalHeight
        };
    }

    public static void Deserialize(this Board board, BoardEncoding encoding, out int piecesOnBoard)
    {
        if (encoding.Width != board.Width)
            throw new Exception("Invalid Width");
        if (encoding.Height != board.Height)
            throw new Exception("Invalid Height");

        board.Reset();

        int x = 0;
        int y = 0;
        piecesOnBoard = 0;

        foreach (var ch in encoding.Code)
        {
            if (ch != ' ')
            {
                if (ch == 'X')
                {
                    board.SetCellAt(x, y, BoardCellType.Garbage);
                }
                else if (ch == '#')
                {
                    board.SetCellAt(x, y, BoardCellType.Solid);
                }
                else
                {
                    var res = (BoardCellType)Enum.Parse(typeof(BoardCellType), ch.ToString());
                    board.SetCellAt(x, y, res);
                }

                piecesOnBoard++;
            }

            x++;
            if (x >= board.Width)
            {
                y++;
                x = 0;
            }
        }
    }

    public static Board Deserialize(BoardEncoding encoding, out int piecesOnBoard)
    {
        var board = new Board(encoding.Width, encoding.Height);
        Deserialize(board, encoding, out piecesOnBoard);

        return board;
    }
}