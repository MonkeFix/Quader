using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez;
using Nez.Sprites;
using Nez.UI;
using Quader.Components.Boards;
using Quader.Components.Boards.PieceHandlers;
using Quader.Components.Boards.Renderers;
using Quader.Components.Debugging;
using Quader.Components.UI;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Settings;
using Quader.Managers.TimeProviders;
using Quader.Scenes;
using Quader.Skinning;

namespace Quader.Components
{
    public enum PieceHandlerType
    {
        Player,
        Bot,
        Remote
    }

    public class BoardBuilder
    {
        private static int _boardIndex = 0;
        private Entity _entity;
        private GameSettings? _gameSettings;
        private IPieceGenerator? _pieceGenerator;
        private IPieceHandler? _pieceHandler;
        private PieceHandlerType? _type;
        private Board? _otherBoard;
        private BoardManagerComponent _boardManager;

        public BoardBuilder(BoardManagerComponent boardManager, string? boardName = null)
        {
            if (!string.IsNullOrEmpty(boardName))
                _entity = new Entity(boardName);
            else 
                _entity = new Entity("board_" + _boardIndex);

            _boardManager = boardManager;

            _entity.Tag = GameplayScene.BoardTag;
            _boardIndex++;
        }

        public BoardBuilder AddGameSettings(GameSettings gameSettings)
        {
            _gameSettings = gameSettings;

            return this;
        }

        public BoardBuilder AddPieceGenerator(IPieceGenerator pieceGenerator)
        {
            _pieceGenerator = pieceGenerator;

            return this;
        }

        public BoardBuilder AddPieceHandler(PieceHandlerType type)
        {
            _type = type;

            return this;
        }

        public BoardBuilder AddPvpController(Board other)
        {
            _otherBoard = other;

            return this;
        }

        public BoardBuilder SetPosition(Vector2 position)
        {
            _entity.Position = position;

            return this;
        }

        public BoardHolder Build(bool startDisabled = true)
        {
            if (_gameSettings == null)
                _gameSettings = GameSettings.Default;
            if (_pieceGenerator == null)
                _pieceGenerator = new PieceGeneratorBag7(5);

            var board = new Board(_gameSettings);
            var boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            var boardRenderer = _entity.AddComponent(new SpriteRenderer(boardSkin.BoardTexture));
            boardRenderer.Origin = new Vector2(188, 0);
            boardRenderer.LayerDepth = 1f;

            var components = new List<Component>()
            {
                new HeldPieceComponent(board),
                new LoseWinHandlerComponent(board, _boardManager),
                new BoardGridRendererComponent(board),
                new BoardRendererComponent(board),
                new PieceRendererComponent(board),
#if DEBUG
                new BoardStateRenderer(board),
#endif
                new PieceQueueComponent(board, _pieceGenerator),
                
                new ScoreHandlerComponent(board),
                new BoardGravityComponent(board),
                new DamageMeterComponent(board)
            };

            switch (_type)
            {
                case PieceHandlerType.Player:
                    var ph1 = new PieceHandlerPlayerComponent(board);
                    _pieceHandler = ph1;
                    components.Add(ph1);

#if DEBUG
                    components.Add(new BoardImGuiComponent(board));
                    components.Add(new LogsImGuiComponent());
                    components.Add(new ProfilerImGuiComponent());
#endif

                    components.Add(new TimeManagerComponent(board, new LocalTimeProvider()));

                    break;
                case PieceHandlerType.Bot:
                    var ph2 = new PieceHandlerBotLocalComponent(board); //PieceHandlerBotComponent(board);
                    _pieceHandler = ph2;
                    components.Add(ph2);

                    components.Add(new TimeManagerComponent(board, new LocalTimeProvider()));
                    break;
                case PieceHandlerType.Remote:
                    var ph3 = new PieceHandlerRemoteComponent(board);
                    _pieceHandler = ph3;
                    components.Add(ph3);

                    // TODO: Add time provider
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }


            if (_otherBoard != null)
                components.Add(new PvpControllerComponent(board, _otherBoard));

            components.Add(new ReplayComponent(board));
            components.Add(new PracticeModeUiComponent(board));
            
            _entity.AddComponents(components);

            /*void RestartAction()
            {
                foreach (var component in components)
                {
                    if (component is IResetable resetable) resetable.Reset();
                }
            }*/

            // board.StartMoveHolder(Time.FrameCount);

            if (startDisabled)
            {
                foreach (var component in components)
                {
                    if (component is IBoardToggleable bt)
                        bt.Disable();
                }
            }

            return new BoardHolder(board, _entity, components, _pieceHandler);
        }
    }
}