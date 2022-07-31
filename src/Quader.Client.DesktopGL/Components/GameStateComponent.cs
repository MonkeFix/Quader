using Nez;
using Nez.AI.FSM;
using Quader.Components.Boards;
using Quader.Debugging.Logging;
using Quader.Engine.Replays;

namespace Quader.Components
{
    public class GameStateMachineComponent : SimpleStateMachine<GameState>, IResetable
    {
        private readonly ILogger _logger = LoggerFactory.GetLogger<GameStateMachineComponent>();

        public GameStateMachineComponent()
        {
            
        }

        public void StartGame()
        {
            CurrentState = GameState.GameOngoing;
        }

        public void EndGame()
        {
            CurrentState = GameState.PostGame;
        }

        public void Reset()
        {
            CurrentState = GameState.PreGame;
        }

        public override void OnAddedToEntity()
        {
            InitialState = GameState.PreGame;
            CurrentState = GameState.PreGame;
        }

        protected void PreGame_Enter()
        {
            _logger.Debug("PreGame_Enter");
        }
        protected void PreGame_Tick()
        {
            //_logger.Debug("PreGame_Tick");
        }
        protected void PreGame_Exit()
        {
            _logger.Debug("PreGame_Exit");
        }

        protected void GameOngoing_Enter()
        {
            _logger.Debug("GameOngoing_Enter");
        }
        protected void GameOngoing_Tick()
        {
            //_logger.Debug("GameOngoing_Tick");
        }
        protected void GameOngoing_Exit()
        {
            _logger.Debug("GameOngoing_Exit");
        }

        protected void PostGame_Enter()
        {
            _logger.Debug("PostGame_Enter");
        }
        protected void PostGame_Tick()
        {
            //_logger.Debug("PostGame_Tick");
        }
        protected void PostGame_Exit()
        {
            _logger.Debug("PostGame_Exit");
        }
    }
}