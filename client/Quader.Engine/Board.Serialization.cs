using System;
using Quader.Engine.Serialization;

namespace Quader.Engine;

public partial class Board
{
    public BoardEncoding Encode()
    {
        return this.Serialize();
    }

    public void Decode(BoardEncoding encoding)
    {
        this.Deserialize(encoding, out _piecesOnBoard);

        BoardChanged?.Invoke(this, EventArgs.Empty);
    }
}