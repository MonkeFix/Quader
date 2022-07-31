using System;
using Microsoft.Xna.Framework;
using Nez;
using Nez.Sprites;
using Nez.UI;
using Quader.Components.Boards;
using Quader.Components.Boards.Renderers;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Settings;
using Quader.Skinning;

namespace Quader.Components
{
    public enum PieceHandlerType
    {
        Player,
        Bot
    }

    public class BoardFactory
    {
        private static int _boardIndex = 0;
        private Entity _entity;
        private GameSettings? _gameSettings;
        private IPieceGenerator? _pieceGenerator;
        private IPieceHandler? _pieceHandler;
        private PieceHandlerType? _type;
        private Board? _otherBoard;

        public BoardFactory()
        {
            _entity = new Entity("board_" + _boardIndex);
            _boardIndex++;
        }

        public BoardFactory AddGameSettings(GameSettings gameSettings)
        {
            _gameSettings = gameSettings;

            return this;
        }

        public BoardFactory AddPieceGenerator(IPieceGenerator pieceGenerator)
        {
            _pieceGenerator = pieceGenerator;

            return this;
        }

        public BoardFactory AddPieceHandler(PieceHandlerType type)
        {
            _type = type;

            return this;
        }

        public BoardFactory AddPvpController(Board other)
        {
            _otherBoard = other;

            return this;
        }

        public BoardFactory SetPosition(Vector2 position)
        {
            _entity.Position = position;

            return this;
        }

        public Entity Build(out Board board)
        {
            if (_gameSettings == null)
                _gameSettings = GameSettings.Default;
            if (_pieceGenerator == null)
                _pieceGenerator = new PieceGeneratorBag7(5);

            board = new Board(_gameSettings);
            var boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            var boardRenderer = _entity.AddComponent(new SpriteRenderer(boardSkin.BoardTexture));
            boardRenderer.Origin = new Vector2(188, 0);
            boardRenderer.LayerDepth = 1f;

            Component[] components =
            {
                new BoardGridRendererComponent(board),
                new BoardRendererComponent(board),
                new PieceRendererComponent(board),
#if DEBUG
                new BoardImGuiComponent(board),
#endif
                new PieceQueueComponent(board, _pieceGenerator),
                new HeldPieceComponent(board),
                new ScoreHandlerComponent(board),
                new BoardGravityComponent(board),
                new DamageMeterComponent(board)
            };

            _entity.AddComponents(components);

            void RestartAction()
            {
                foreach (var component in components)
                {
                    if (component is IResetable resetable) resetable.Reset();
                }
            }

            switch (_type)
            {
                case PieceHandlerType.Player:
                    _entity.AddComponent(new PieceHandlerPlayerComponent(board, RestartAction));
                    break;
                case PieceHandlerType.Bot:
                    _entity.AddComponent(new PieceHandlerBotComponent(board));
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }

            _entity.AddComponent(new LoseHandlerComponent(board, RestartAction));

            if (_otherBoard != null)
                _entity.AddComponent(new PvpControllerComponent(board, _otherBoard));

            /*if (_pieceHandler == null)
                _pieceHandler = new PieceHandlerPlayerComponent(board, () => { });*/

            return _entity;
        }
    }
}