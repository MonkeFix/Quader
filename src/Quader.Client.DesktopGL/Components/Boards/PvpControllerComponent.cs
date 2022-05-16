using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class PvpControllerComponent : Component, IBoardComponent, IUpdatable
    {
        public Board Board { get; }
        public Board BoardOpponent { get; }

        public PvpControllerComponent(Board boardPlayer, Board boardOpponent)
        {
            Board = boardPlayer;
            BoardOpponent = boardOpponent;

            Board.PieceHardDropped += (sender, move) =>
            {
                if (move.LinesCleared > 0)
                    BoardOpponent.Attack(move);
            };

            BoardOpponent.PieceHardDropped += (sender, move) =>
            {
                if (move.LinesCleared > 0)
                    Board.Attack(move);
            };
        }


        public void Update()
        {
            
        }
    }
}