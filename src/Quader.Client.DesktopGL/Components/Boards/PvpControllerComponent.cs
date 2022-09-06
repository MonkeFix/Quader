using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class PvpControllerComponent : Component, IBoardComponent, IUpdatable, IBoardToggleable
    {
        public Board Board { get; }
        public Board BoardOpponent { get; }

        public PvpControllerComponent(Board boardPlayer, Board boardOpponent)
        {
            Board = boardPlayer;
            BoardOpponent = boardOpponent;

            Board.PieceHardDropped += (sender, move) =>
            {
                if (Enabled && move.LinesCleared > 0)
                    BoardOpponent.Attack(move);
            };

            BoardOpponent.PieceHardDropped += (sender, move) =>
            {
                if (Enabled && move.LinesCleared > 0)
                    Board.Attack(move);
            };
        }


        public void Update()
        {
            
        }

        public void Enable()
        {
            Enabled = true;
        }

        public void Disable()
        {
            Enabled = false;
        }

        public void Toggle()
        {
            Enabled = !Enabled;
        }
    }
}