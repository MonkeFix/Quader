using System;
using System.IO;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Sprites;
using Nez.Timers;
using Nez.UI;
using Quader.Components;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;
using Quader.Engine.Settings;
using Quader.Serialization;
using Quader.Skinning;

namespace Quader.Scenes
{
    public class GameplayScene : Scene
    {
        public const int ScreenSpaceRenderLayer = 999;
        public const int BoardTag = 1000;

        public readonly int Width = 1600;
        public readonly int Height = 980;

        private readonly ILogger _logger = LoggerFactory.GetLogger<GameplayScene>();

        private BoardManagerComponent _boardManager;

        public GameplayScene()
        {
            _logger.Debug("Constructing, adding renderer");
            AddRenderer(new DefaultRenderer(0));
            AddRenderer(new ScreenSpaceRenderer(100, ScreenSpaceRenderLayer));
            AddRenderer(new RenderLayerExcludeRenderer(0, ScreenSpaceRenderLayer));
        }

        public override void Initialize()
        {
            base.Initialize();
            ClearColor = Color.Black; 

            _logger.Debug("Initializing");

            SetDesignResolution(Width, Height, SceneResolutionPolicy.BestFit);
            Screen.SetSize(1920, 1080);

            var skin = Core.Services.GetService<Skin>();

            /*var ui = new Entity("ui");
            var canvas = ui.AddComponent(new UICanvas());

            var table = canvas.Stage.AddElement(new Table());
            var b = table.Add(new TextButton("Hello!", skin));
            b.Width(256);
            b.Height(64);

            AddEntity(ui);*/

            _logger.Debug("Creating Board Entity...");

            var queueSize = 5;
            var pieceGenerator = new PieceGeneratorBag7(queueSize);

            var boardSkin = skin.Get<BoardSkin>();

            var boards = BuildBoards();

            _boardManager = CreateEntity("game_state_machine")
                .AddComponent(new BoardManagerComponent(boards));

            /*Core.Schedule(2f, true, boardBot, (timer) =>
            {
                timer.GetContext<Board>().PushGarbage(1);
            });

            _logger.Debug("Done initializing");*/
        }

        public override void Update()
        {
            base.Update();
        }

        private BoardHolder[] BuildBoards()
        {
            //DestroyBoards();

            var gameSettings = GameSettings.Default;

            var boardPlayer =
                new BoardBuilder()
                    .AddGameSettings(gameSettings)
                    .AddPieceHandler(PieceHandlerType.Player)
                    .SetPosition(new Vector2(200, 128))
                    .Build();

            var boardBot =
                new BoardBuilder()
                    .AddGameSettings(gameSettings)
                    .AddPieceHandler(PieceHandlerType.Bot)
                    .SetPosition(new Vector2(256 + 512 + 128 + 64, 128))
                    .AddPvpController(boardPlayer.Board)
                    .Build();

            AddEntity(boardPlayer.BoardEntity);
            AddEntity(boardBot.BoardEntity);

            return new[]
            {
                boardPlayer,
                boardBot
            };
        }

        private void DestroyBoards()
        {
            var entities = FindEntitiesWithTag(BoardTag);
            foreach (var entity in entities)
            {
                entity.Destroy();
            }
        }
    }
}