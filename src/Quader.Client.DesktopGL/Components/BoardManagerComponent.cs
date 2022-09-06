using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.AI.FSM;
using Quader.Components.Boards;
using Quader.Components.Boards.PieceHandlers;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Replays;

namespace Quader.Components
{
    public class BoardManagerComponent : SimpleStateMachine<GameState>, IResetable, IUpdatable
    {
        private readonly ILogger _logger = LoggerFactory.GetLogger<BoardManagerComponent>();

        private IEnumerable<BoardHolder> _boards;
        private IEnumerable<IPieceHandler> _pieceHandlers;

        public GameState State => CurrentState;

        public BoardManagerComponent(IEnumerable<BoardHolder> boards)
        {
            _boards = boards;
        }
        
        public BoardManagerComponent(BoardHolder board) : this(new[] { board })
        { }

        public override void Update()
        {
            base.Update();

            if (Input.IsKeyPressed(Keys.R))
            {
                if (State == GameState.PreGame)
                    StartGame();
                else if (State == GameState.GameOngoing)
                    Reset();
                else if (State == GameState.PostGame)
                    StartGame();
            }
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
            //CurrentState = GameState.PreGame;
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
            foreach (var board in _boards)
            {
                board.Start();
                board.Enable();
            }
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