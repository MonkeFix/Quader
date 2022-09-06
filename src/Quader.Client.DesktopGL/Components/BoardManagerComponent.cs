using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.AI.FSM;
using Nez.Persistence;
using Quader.Components.Boards;
using Quader.Components.Boards.PieceHandlers;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Replays;
using Quader.Engine.Settings;
using Quader.Scenes;

namespace Quader.Components
{
    public class BoardManagerComponent : SimpleStateMachine<GameState>, IUpdatable
    {
        private readonly ILogger _logger = LoggerFactory.GetLogger<BoardManagerComponent>();

        private IEnumerable<BoardHolder>? _boards;
        private IEnumerable<IPieceHandler> _pieceHandlers;

        public GameState State => CurrentState;

        public override void Update()
        {
            base.Update();

            if (Input.IsKeyPressed(Keys.R))
            {
                if (State == GameState.PreGame)
                    CurrentState = GameState.GameOngoing;
                else if (State == GameState.GameOngoing)
                    CurrentState = GameState.PreGame;
                else if (State == GameState.PostGame)
                    CurrentState = GameState.PreGame;
            }

            if (Input.IsKeyPressed(Keys.P))
            {
                var boardsArr = _boards.ToArray();

                for (int i = 0; i < boardsArr.Length; i++)
                {
                    var b = boardsArr[i];

                    var json = Json.ToJson(b.Board.StopReplay(), true);
                    File.WriteAllText($"REPLAY_{i}.json", json);
                }
            }
        }

        public override void OnAddedToEntity()
        {
            InitialState = GameState.PreGame;
            //CurrentState = GameState.PreGame;
        }

        public override void DebugRender(Batcher batcher)
        {
            base.DebugRender(batcher);

            batcher.DrawString(Graphics.Instance.BitmapFont, $"Current State: {State}", new Vector2(256, 64), Color.White);
        }

        protected void PreGame_Enter()
        {
            _logger.Debug("PreGame_Enter");
            if (_boards != null)
            {
                foreach (var board in _boards)
                {
                    board.BoardEntity.Destroy();
                }

                var allBoards = Entity.Scene.FindEntitiesWithTag(GameplayScene.BoardTag);
                foreach (var b in allBoards)
                {
                    b.Destroy();
                }

                _boards = null;
            }
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

            _boards = BuildBoards(2);
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

            if (_boards != null)
            {
                foreach (var bh in _boards)
                {
                    bh.Disable();
                }
            }
        }
        protected void PostGame_Tick()
        {
            //_logger.Debug("PostGame_Tick");
        }
        protected void PostGame_Exit()
        {
            _logger.Debug("PostGame_Exit");
        }

        private BoardHolder BuildBoard(
            GameSettings gameSettings, 
            PieceHandlerType pieceHandlerType, 
            Vector2 position,
            Board? pvpBoard = null
            )
        {
            var bb = new BoardBuilder()
                .AddGameSettings(gameSettings)
                .AddPieceHandler(pieceHandlerType)
                .SetPosition(position);

            if (pvpBoard != null)
                bb.AddPvpController(pvpBoard);

            return bb.Build();
        }

        private BoardHolder[] BuildBoards(int count)
        {
            var gameSettings = GameSettings.Default;

            var result = new BoardHolder[count];

            for (int i = 0; i < count; i++)
            {
                BoardHolder board;

                if (i == 0)
                {
                    board = new BoardBuilder()
                        .AddGameSettings(gameSettings)
                        .AddPieceHandler(PieceHandlerType.Player)
                        .SetPosition(new Vector2(200, 128))
                        .Build();
                }
                else
                {
                    board =
                        new BoardBuilder()
                            .AddGameSettings(gameSettings)
                            .AddPieceHandler(PieceHandlerType.Bot)
                            .SetPosition(new Vector2(256 + 512 + 128 + 64, 128))
                            .AddPvpController(result[0].Board)
                            .Build();
                }

                board.Board.PieceCannotBeSpawned += (sender, args) =>
                {
                    CurrentState = GameState.PostGame;
                };
                result[i] = board;
            }

            /*Core.Schedule(2f, true, boardBot, (context) =>
            {
                var board = context.GetContext<BoardHolder>();
                if (board.IsEnabled)
                    board.Board.PushGarbage(1);
            });*/

            foreach (var boardHolder in result)
            {
                Entity.Scene.AddEntity(boardHolder.BoardEntity);
            }

            return result;
        }
    }
}