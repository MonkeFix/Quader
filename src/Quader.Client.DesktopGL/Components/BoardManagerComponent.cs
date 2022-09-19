using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.AI.FSM;
using Nez.Persistence;
using Nez.Persistence.Binary;
using Quader.Components.Boards;
using Quader.Components.Boards.PieceHandlers;
using Quader.Components.UI;
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

        private BoardHolder[]? _boards;
 
        public GameState State => CurrentState;

        private SharedActions _sharedActions;

        public float PrepareTimer { get; set; } = 3; // in seconds
        public float CurrentTimerValue { get; private set; }

        public BoardManagerComponent(SharedActions sharedActions)
        {
            _sharedActions = sharedActions;

            sharedActions.NewGameAction = () => CurrentState = GameState.PreGame;
            sharedActions.RestartAction = () => Restart();
            sharedActions.QuitAction = () => Core.Exit();
        }

        public override void Update()
        {
            base.Update();

            if (Input.IsKeyPressed(Keys.R))
            {
                Restart();
            }

            /*if (Input.IsKeyPressed(Keys.P) && _boards != null)
            {
                var boardsArr = _boards.ToArray();

                for (int i = 0; i < boardsArr.Length; i++)
                {
                    var b = boardsArr[i];

                    var json = Json.ToJson(b.Board.StopMoveHolder(), true);
                    File.WriteAllText($"REPLAY_{i}.json", json);
                }
            }*/
        }

        public override void OnAddedToEntity()
        {
            InitialState = GameState.PreGame;
            //CurrentState = GameState.PreGame;
        }

        public override void DebugRender(Batcher batcher)
        {
            base.DebugRender(batcher);

            batcher.DrawString(
                Graphics.Instance.BitmapFont,
                $"Current State: {State}\n" +
                $"Timer: {CurrentTimerValue}",
                new Vector2(256, 64),
                Color.White
            );
        }

        private void Restart()
        {
            CurrentState = GameState.GameTimer;
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

        protected void GameTimer_Enter()
        {
            CurrentTimerValue = PrepareTimer;

            if (_boards != null)
            {
                var allBoards = Entity.Scene.FindEntitiesWithTag(GameplayScene.BoardTag);
                foreach (var b in allBoards)
                {
                    b.RemoveAllComponents();
                    b.Destroy();
                }

                foreach (var board in _boards)
                {
                    board.Board.Dispose();
                    // board.BoardEntity.Destroy();
                }

                _boards = null;

                GC.Collect();
            }

            _boards = BuildBoards(2);
        }

        protected void GameTimer_Tick()
        {
            CurrentTimerValue -= Time.DeltaTime;
            if (CurrentTimerValue <= 0)
                CurrentState = GameState.GameOngoing;
        }

        protected void GameTimer_Exit()
        {
            CurrentTimerValue = PrepareTimer;
        }

        protected void GameOngoing_Enter()
        {
            _logger.Debug("GameOngoing_Enter");

            foreach (var board in _boards)
            {
                board.Enable();
                board.Start();
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

            var dataStore = Core.Services.GetService<FileDataStore>();

            if (_boards != null)
            {
                foreach (var bh in _boards)
                {
                    var replay = bh.BoardEntity.GetComponent<ReplayComponent>().End();
                    var jsonStr = Json.ToJson(replay);
                    var dir = $"Replays/{replay.EndDate:yyyy-MM-dd-HH-mm-ss}";
                    Directory.CreateDirectory("Saves/" + dir);
                    File.WriteAllText($"Saves/{dir}/{bh.BoardEntity.Name}.json", jsonStr);

                    // testing persistence
                    dataStore.Save($"{dir}/{bh.BoardEntity.Name}.bin", replay);
                    var tmpReplay = new BoardMoveHolder(bh.Board, DateTime.UtcNow);
                    dataStore.Load($"{dir}/{bh.BoardEntity.Name}.bin", tmpReplay);

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
            var bb = new BoardBuilder(this)
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
                    board = new BoardBuilder(this)
                        .AddGameSettings(gameSettings)
                        .AddPieceHandler(PieceHandlerType.Player)
                        .SetPosition(new Vector2(200, 128))
                        .Build();
                }
                else
                {
                    board =
                        new BoardBuilder(this)
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