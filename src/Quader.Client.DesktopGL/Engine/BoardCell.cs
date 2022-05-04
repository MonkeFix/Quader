using Microsoft.Xna.Framework;
using Quader.Engine.Pieces;

namespace Quader.Engine
{
    public class BoardCell
    {
        public Point Position { get; }
        public BoardCellType Type { get; }
        public Color BaseColor { get; }

        public BoardCell(Point position, BoardCellType type)
        {
            Position = position;
            Type = type;
            BaseColor = PieceUtils.GetColorByBoardPieceType(Type);
        }
    }
}