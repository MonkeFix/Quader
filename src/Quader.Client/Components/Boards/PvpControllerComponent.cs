using LiteLog.Logging;
using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class PvpControllerComponent : Component, IBoardComponent, IBoardToggleable
    {
        public Board Board { get; }
        public Board BoardOpponent { get; }
        
        private readonly ILogger _logger = LoggerFactory.GetLogger<PvpControllerComponent>();

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

        public void Enable()
        {
            _logger.Trace("Enabling");
            Enabled = true;
        }

        public void Disable()
        {
            _logger.Trace("Disabling");
            Enabled = false;
        }
    }
}